{-# LANGUAGE DuplicateRecordFields #-}

module Aws.Lambda.Runtime.ApiGatewayInfo
  ( ApiGatewayRequest(..)
  , ApiGatewayResponse(..)
  , mkApiGatewayResponse ) where

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Aeson.Types (Parser)

-- TODO: Add the rest of the fields
data ApiGatewayRequest body = ApiGatewayRequest
  { apiGatewayRequestResource              :: !Text
  , apiGatewayRequestPath                  :: !Text
  , apiGatewayRequestHttpMethod            :: !Text
  , apiGatewayRequestHeaders               :: !(HashMap Text Text)
  , apiGatewayRequestQueryStringParameters :: !(Maybe [(Text, Maybe Text)])
  , apiGatewayRequestPathParameters        :: !(Maybe (HashMap Text Text))
  , apiGatewayRequestStageVariables        :: !(Maybe (HashMap Text Text))
  , apiGatewayRequestBody                  :: !body
  } deriving (Generic, Show)

instance FromJSON body => FromJSON (ApiGatewayRequest body) where
  parseJSON (Object v) = ApiGatewayRequest <$>
    v .: "resource" <*>
    v .: "path" <*>
    v .: "httpMethod" <*>
    v .: "headers" <*>
    v .: "queryStringParameters" <*>
    v .: "pathParameters" <*>
    v .: "stageVariables" <*>
    v `parseObjectFromStringField` "body"
  parseJSON _ = fail "Expected ApiGatewayRequest to be an object."

-- We need this because API Gateway is going to send us the payload as a JSON string
parseObjectFromStringField :: FromJSON a => Object -> Text -> Parser a
parseObjectFromStringField obj fieldName = do
  fieldContents <- obj .: fieldName
  case fieldContents of
    String stringContents ->
      case eitherDecodeStrict (T.encodeUtf8 stringContents) of
        Right success -> pure success
        Left err -> fail err
    other -> T.typeMismatch "String" other

data ApiGatewayResponse body = ApiGatewayResponse
  { apiGatewayResponseStatusCode      :: !Int
  , apiGatewayResponseHeaders         :: !ResponseHeaders
  , apiGatewayResponseBody            :: !body
  , apiGatewayResponseIsBase64Encoded :: !Bool
  } deriving (Generic, Show)

instance Functor ApiGatewayResponse where
  fmap f v = v { apiGatewayResponseBody = f (apiGatewayResponseBody v) }

instance ToJSON body => ToJSON (ApiGatewayResponse body)  where
  toJSON ApiGatewayResponse {..} = object
    [ "statusCode" .= apiGatewayResponseStatusCode
    , "body" .= (String . T.pack . LazyByteString.unpack . encode @body $ apiGatewayResponseBody)
    , "headers" .= object (map headerToPair apiGatewayResponseHeaders)
    , "isBase64Encoded" .= apiGatewayResponseIsBase64Encoded
    ]

mkApiGatewayResponse :: Int -> payload -> ApiGatewayResponse payload
mkApiGatewayResponse code payload =
  ApiGatewayResponse code [] payload False

headerToPair :: Header -> T.Pair
headerToPair (cibyte, bstr) = k .= v
 where
  k = (T.decodeUtf8 . CI.original) cibyte
  v = T.decodeUtf8 bstr