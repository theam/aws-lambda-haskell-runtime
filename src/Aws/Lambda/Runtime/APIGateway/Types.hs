{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Aws.Lambda.Runtime.APIGateway.Types
  ( ApiGatewayRequest (..),
    ApiGatewayRequestContext (..),
    ApiGatewayRequestContextIdentity (..),
    ApiGatewayResponse (..),
    ApiGatewayResponseBody (..),
    ToApiGatewayResponseBody (..),
    ApiGatewayDispatcherOptions (..),
    mkApiGatewayResponse,
  )
where

import Aws.Lambda.Utilities (toJSONText)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Object,
    ToJSON (toJSON),
    Value (Null, Object, String),
    eitherDecodeStrict,
    object,
    (.:),
    (.:?)
  )
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.Types as T
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.HTTP.Types (Header, ResponseHeaders)

-- | API Gateway specific dispatcher options
newtype ApiGatewayDispatcherOptions = ApiGatewayDispatcherOptions
  { -- | Should impure exceptions be propagated through the API Gateway interface
    propagateImpureExceptions :: Bool
  }

data ApiGatewayRequest body = ApiGatewayRequest
  { apiGatewayRequestResource :: !Text,
    apiGatewayRequestPath :: !Text,
    apiGatewayRequestHttpMethod :: !Text,
    apiGatewayRequestHeaders :: !(Maybe (HashMap Text Text)),
    apiGatewayRequestQueryStringParameters :: !(Maybe (HashMap Text Text)),
    apiGatewayRequestPathParameters :: !(Maybe (HashMap Text Text)),
    apiGatewayRequestStageVariables :: !(Maybe (HashMap Text Text)),
    apiGatewayRequestIsBase64Encoded :: !Bool,
    apiGatewayRequestRequestContext :: !ApiGatewayRequestContext,
    apiGatewayRequestBody :: !(Maybe body)
  }
  deriving (Show)

-- We special case String and Text in order
-- to avoid unneeded encoding which will wrap them in quotes and break parsing
instance {-# OVERLAPPING #-} FromJSON (ApiGatewayRequest Text) where
  parseJSON = parseApiGatewayRequest (.:)

instance {-# OVERLAPPING #-} FromJSON (ApiGatewayRequest String) where
  parseJSON = parseApiGatewayRequest (.:)

instance FromJSON body => FromJSON (ApiGatewayRequest body) where
  parseJSON = parseApiGatewayRequest parseObjectFromStringField

-- We need this because API Gateway is going to send us the payload as a JSON string
parseObjectFromStringField :: FromJSON a => Object -> T.Key -> Parser (Maybe a)
parseObjectFromStringField obj fieldName = do
  fieldContents <- obj .: fieldName
  case fieldContents of
    String stringContents ->
      case eitherDecodeStrict (T.encodeUtf8 stringContents) of
        Right success -> pure success
        Left err -> fail err
    Null -> pure Nothing
    other -> T.typeMismatch "String or Null" other

parseApiGatewayRequest :: (Object -> T.Key -> Parser (Maybe body)) -> Value -> Parser (ApiGatewayRequest body)
parseApiGatewayRequest bodyParser (Object v) =
  ApiGatewayRequest
    <$> v .: "resource"
    <*> v .: "path"
    <*> v .: "httpMethod"
    <*> v .: "headers"
    <*> v .: "queryStringParameters"
    <*> v .: "pathParameters"
    <*> v .: "stageVariables"
    <*> v .: "isBase64Encoded"
    <*> v .: "requestContext"
    <*> v `bodyParser` "body"
parseApiGatewayRequest _ _ = fail "Expected ApiGatewayRequest to be an object."

data ApiGatewayRequestContext = ApiGatewayRequestContext
  { apiGatewayRequestContextResourceId :: !Text,
    apiGatewayRequestContextResourcePath :: !Text,
    apiGatewayRequestContextHttpMethod :: !Text,
    apiGatewayRequestContextExtendedRequestId :: !(Maybe Text),
    apiGatewayRequestContextRequestTime :: !Text,
    apiGatewayRequestContextPath :: !Text,
    apiGatewayRequestContextAccountId :: !Text,
    apiGatewayRequestContextProtocol :: !Text,
    apiGatewayRequestContextStage :: !Text,
    apiGatewayRequestContextDomainPrefix :: !(Maybe Text),
    apiGatewayRequestContextRequestId :: !Text,
    apiGatewayRequestContextDomainName :: !(Maybe Text),
    apiGatewayRequestContextApiId :: !Text,
    apiGatewayRequestContextIdentity :: !ApiGatewayRequestContextIdentity,
    apiGatewayRequestContextAuthorizer :: !(Maybe Value)
  }
  deriving (Show)

instance FromJSON ApiGatewayRequestContext where
  parseJSON (Object v) =
    ApiGatewayRequestContext
      <$> v .: "resourceId"
      <*> v .: "path"
      <*> v .: "httpMethod"
      <*> v .:? "extendedRequestId"
      <*> v .: "requestTime"
      <*> v .: "path"
      <*> v .: "accountId"
      <*> v .: "protocol"
      <*> v .: "stage"
      <*> v .:? "domainPrefix"
      <*> v .: "requestId"
      <*> v .:? "domainName"
      <*> v .: "apiId"
      <*> v .: "identity"
      <*> v .:? "authorizer"
  parseJSON _ = fail "Expected ApiGatewayRequestContext to be an object."

data ApiGatewayRequestContextIdentity = ApiGatewayRequestContextIdentity
  { apiGatewayRequestContextIdentityCognitoIdentityPoolId :: !(Maybe Text),
    apiGatewayRequestContextIdentityAccountId :: !(Maybe Text),
    apiGatewayRequestContextIdentityCognitoIdentityId :: !(Maybe Text),
    apiGatewayRequestContextIdentityCaller :: !(Maybe Text),
    apiGatewayRequestContextIdentitySourceIp :: !(Maybe Text),
    apiGatewayRequestContextIdentityPrincipalOrgId :: !(Maybe Text),
    apiGatewayRequestContextIdentityAccesskey :: !(Maybe Text),
    apiGatewayRequestContextIdentityCognitoAuthenticationType :: !(Maybe Text),
    apiGatewayRequestContextIdentityCognitoAuthenticationProvider :: !(Maybe Value),
    apiGatewayRequestContextIdentityUserArn :: !(Maybe Text),
    apiGatewayRequestContextIdentityUserAgent :: !(Maybe Text),
    apiGatewayRequestContextIdentityUser :: !(Maybe Text)
  }
  deriving (Show)

instance FromJSON ApiGatewayRequestContextIdentity where
  parseJSON (Object v) =
    ApiGatewayRequestContextIdentity
      <$> v .: "cognitoIdentityPoolId"
      <*> v .: "accountId"
      <*> v .:? "cognitoIdentityId"
      <*> v .: "caller"
      <*> v .: "sourceIp"
      <*> v .:? "principalOrgId"
      <*> v .:? "accessKey"
      <*> v .: "cognitoAuthenticationType"
      <*> v .: "cognitoAuthenticationProvider"
      <*> v .: "userArn"
      <*> v .: "userAgent"
      <*> v .: "user"
  parseJSON _ = fail "Expected ApiGatewayRequestContextIdentity to be an object."

newtype ApiGatewayResponseBody
  = ApiGatewayResponseBody Text
  deriving newtype (ToJSON, FromJSON)

class ToApiGatewayResponseBody a where
  toApiGatewayResponseBody :: a -> ApiGatewayResponseBody

-- We special case Text and String to avoid unneeded encoding which will wrap them in quotes
instance {-# OVERLAPPING #-} ToApiGatewayResponseBody Text where
  toApiGatewayResponseBody = ApiGatewayResponseBody

instance {-# OVERLAPPING #-} ToApiGatewayResponseBody String where
  toApiGatewayResponseBody = ApiGatewayResponseBody . T.pack

instance ToJSON a => ToApiGatewayResponseBody a where
  toApiGatewayResponseBody = ApiGatewayResponseBody . toJSONText

data ApiGatewayResponse body = ApiGatewayResponse
  { apiGatewayResponseStatusCode :: !Int,
    apiGatewayResponseHeaders :: !ResponseHeaders,
    apiGatewayResponseBody :: !body,
    apiGatewayResponseIsBase64Encoded :: !Bool
  }
  deriving (Generic, Show)

instance Functor ApiGatewayResponse where
  fmap f v = v {apiGatewayResponseBody = f (apiGatewayResponseBody v)}

instance ToJSON body => ToJSON (ApiGatewayResponse body) where
  toJSON = apiGatewayResponseToJSON toJSON

apiGatewayResponseToJSON :: (body -> Value) -> ApiGatewayResponse body -> Value
apiGatewayResponseToJSON bodyTransformer ApiGatewayResponse {..} =
  object
    [ "statusCode" .= apiGatewayResponseStatusCode,
      "body" .= bodyTransformer apiGatewayResponseBody,
      "headers" .= object (map headerToPair apiGatewayResponseHeaders),
      "isBase64Encoded" .= apiGatewayResponseIsBase64Encoded
    ]

mkApiGatewayResponse :: Int -> ResponseHeaders -> payload -> ApiGatewayResponse payload
mkApiGatewayResponse code headers payload =
  ApiGatewayResponse code headers payload False

headerToPair :: Header -> T.Pair
headerToPair (cibyte, bstr) = k .= v
  where
    k = (K.fromText . T.decodeUtf8 . CI.original) cibyte
    v = T.decodeUtf8 bstr
