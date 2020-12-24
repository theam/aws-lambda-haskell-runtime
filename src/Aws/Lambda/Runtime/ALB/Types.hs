{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Aws.Lambda.Runtime.ALB.Types
  ( ALBRequest (..),
    ALBRequestContext (..),
    ALBResponse (..),
    ALBResponseBody (..),
    ToALBResponseBody (..),
    mkALBResponse,
  )
where

import Aws.Lambda.Utilities (toJSONText, tshow)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Object,
    ToJSON (toJSON),
    Value (Null, Object, String),
    eitherDecodeStrict,
    object,
    (.:),
  )
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as T
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.HTTP.Types (Header, ResponseHeaders)

data ALBRequest body = ALBRequest
  { albRequestPath :: !Text,
    albRequestHttpMethod :: !Text,
    albRequestHeaders :: !(Maybe (HashMap Text Text)),
    -- TODO: They won't be url decoded in ALB
    albRequestQueryStringParameters :: !(Maybe (HashMap Text Text)),
    albRequestIsBase64Encoded :: !Bool,
    albRequestRequestContext :: !ALBRequestContext,
    albRequestBody :: !(Maybe body)
  }
  deriving (Show)

-- We special case String and Text in order
-- to avoid unneeded encoding which will wrap them in quotes and break parsing
instance {-# OVERLAPPING #-} FromJSON (ALBRequest Text) where
  parseJSON = parseALBRequest (.:)

instance {-# OVERLAPPING #-} FromJSON (ALBRequest String) where
  parseJSON = parseALBRequest (.:)

instance FromJSON body => FromJSON (ALBRequest body) where
  parseJSON = parseALBRequest parseObjectFromStringField

-- We need this because API Gateway is going to send us the payload as a JSON string
parseObjectFromStringField :: FromJSON a => Object -> Text -> Parser (Maybe a)
parseObjectFromStringField obj fieldName = do
  fieldContents <- obj .: fieldName
  case fieldContents of
    String stringContents ->
      case eitherDecodeStrict (T.encodeUtf8 stringContents) of
        Right success -> pure success
        Left err -> fail err
    Null -> pure Nothing
    other -> T.typeMismatch "String or Null" other

parseALBRequest :: (Object -> Text -> Parser (Maybe body)) -> Value -> Parser (ALBRequest body)
parseALBRequest bodyParser (Object v) =
  ALBRequest
    <$> v .: "path"
    <*> v .: "httpMethod"
    <*> v .: "headers"
    <*> v .: "queryStringParameters"
    <*> v .: "isBase64Encoded"
    <*> v .: "requestContext"
    <*> v `bodyParser` "body"
parseALBRequest _ _ = fail "Expected ALBRequest to be an object."

newtype ALBRequestContext = ALBRequestContext
  {albRequestContextElb :: ALBELB}
  deriving (Show)

instance FromJSON ALBRequestContext where
  parseJSON (Object v) =
    ALBRequestContext
      <$> v .: "elb"
  parseJSON _ = fail "Expected ALBRequestContext to be an object."

newtype ALBELB = ALBELB
  {albElbTargetGroupArn :: Text}
  deriving (Show)

instance FromJSON ALBELB where
  parseJSON (Object v) =
    ALBELB
      <$> v .: "targetGroupArn"
  parseJSON _ = fail "Expected ALBELB to be an object."

newtype ALBResponseBody
  = ALBResponseBody Text
  deriving newtype (ToJSON, FromJSON)

class ToALBResponseBody a where
  toALBResponseBody :: a -> ALBResponseBody

-- We special case Text and String to avoid unneeded encoding which will wrap them in quotes
instance {-# OVERLAPPING #-} ToALBResponseBody Text where
  toALBResponseBody = ALBResponseBody

instance {-# OVERLAPPING #-} ToALBResponseBody String where
  toALBResponseBody = ALBResponseBody . T.pack

instance ToJSON a => ToALBResponseBody a where
  toALBResponseBody = ALBResponseBody . toJSONText

data ALBResponse body = ALBResponse
  { albResponseStatusCode :: !Int,
    albResponseStatusDescription :: !Text,
    albResponseHeaders :: !ResponseHeaders,
    albResponseMultiValueHeaders :: !ResponseHeaders,
    albResponseBody :: !body,
    albResponseIsBase64Encoded :: !Bool
  }
  deriving (Generic, Show)

instance Functor ALBResponse where
  fmap f v = v {albResponseBody = f (albResponseBody v)}

instance ToJSON body => ToJSON (ALBResponse body) where
  toJSON = albResponseToJSON toJSON

albResponseToJSON :: (body -> Value) -> ALBResponse body -> Value
albResponseToJSON bodyTransformer ALBResponse {..} =
  object
    [ "statusCode" .= albResponseStatusCode,
      "body" .= bodyTransformer albResponseBody,
      "headers" .= object (map headerToPair albResponseHeaders),
      "multiValueHeaders" .= object (map headerToPair albResponseHeaders),
      "isBase64Encoded" .= albResponseIsBase64Encoded
    ]

mkALBResponse :: Int -> ResponseHeaders -> payload -> ALBResponse payload
mkALBResponse code headers payload =
  ALBResponse code (codeDescription code) headers headers payload False
  where
    codeDescription 200 = "200 OK"
    codeDescription 201 = "201 Created"
    codeDescription 202 = "202 Accepted"
    codeDescription 203 = "203 Non-Authoritative Information"
    codeDescription 204 = "204 No Content"
    codeDescription 400 = "400 Bad Request"
    codeDescription 401 = "401 Unauthorized"
    codeDescription 402 = "402 Payment Required"
    codeDescription 403 = "403 Forbidden"
    codeDescription 404 = "404 Not Found"
    codeDescription 405 = "405 Method Not Allowed"
    codeDescription 406 = "406 Not Acceptable"
    codeDescription 500 = "500 Internal Server Error"
    codeDescription other = tshow other

headerToPair :: Header -> T.Pair
headerToPair (cibyte, bstr) = k .= v
  where
    k = (T.decodeUtf8 . CI.original) cibyte
    v = T.decodeUtf8 bstr
