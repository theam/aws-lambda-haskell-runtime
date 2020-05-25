{-# LANGUAGE DuplicateRecordFields #-}

module Aws.Lambda.Runtime.ApiGatewayInfo
  ( ApiGatewayRequest(..)
  , ApiGatewayRequestContext(..)
  , ApiGatewayRequestContextIdentity(..)
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

data ApiGatewayRequest body = ApiGatewayRequest
  { apiGatewayRequestResource              :: !Text
  , apiGatewayRequestPath                  :: !Text
  , apiGatewayRequestHttpMethod            :: !Text
  , apiGatewayRequestHeaders               :: !(HashMap Text Text)
  , apiGatewayRequestQueryStringParameters :: !(Maybe [(Text, Maybe Text)])
  , apiGatewayRequestPathParameters        :: !(Maybe (HashMap Text Text))
  , apiGatewayRequestStageVariables        :: !(Maybe (HashMap Text Text))
  , apiGatewayRequestIsBase64Encoded       :: !Bool
  , apiGatewayRequestRequestContext        :: !ApiGatewayRequestContext
  , apiGatewayRequestBody                  :: !body
  } deriving (Show)

instance FromJSON body => FromJSON (ApiGatewayRequest body) where
  parseJSON (Object v) = ApiGatewayRequest <$>
    v .: "resource" <*>
    v .: "path" <*>
    v .: "httpMethod" <*>
    v .: "headers" <*>
    v .: "queryStringParameters" <*>
    v .: "pathParameters" <*>
    v .: "stageVariables" <*>
    v .: "isBase64Encoded" <*>
    v .: "requestContext" <*>
    v `parseObjectFromStringField` "body"
  parseJSON _ = fail "Expected ApiGatewayRequest to be an object."

data ApiGatewayRequestContext = ApiGatewayRequestContext
  { apiGatewayRequestContextResourceId        :: !Text
  , apiGatewayRequestContextResourcePath      :: !Text
  , apiGatewayRequestContextHttpMethod        :: !Text
  , apiGatewayRequestContextExtendedRequestId :: !Text
  , apiGatewayRequestContextRequestTime       :: !Text
  , apiGatewayRequestContextPath              :: !Text
  , apiGatewayRequestContextAccountId         :: !Text
  , apiGatewayRequestContextProtocol          :: !Text
  , apiGatewayRequestContextStage             :: !Text
  , apiGatewayRequestContextDomainPrefix      :: !Text
  , apiGatewayRequestContextRequestId         :: !Text
  , apiGatewayRequestContextDomainName        :: !Text
  , apiGatewayRequestContextApiId             :: !Text
  , apiGatewayRequestContextIdentity          :: !ApiGatewayRequestContextIdentity
  } deriving (Show)

instance FromJSON ApiGatewayRequestContext where
  parseJSON (Object v) = ApiGatewayRequestContext <$>
    v .: "resourceId" <*>
    v .: "path" <*>
    v .: "httpMethod" <*>
    v .: "extendedRequestId" <*>
    v .: "requestTime" <*>
    v .: "path" <*>
    v .: "accountId" <*>
    v .: "protocol" <*>
    v .: "stage" <*>
    v .: "domainPrefix" <*>
    v .: "requestId" <*>
    v .: "domainName" <*>
    v .: "apiId" <*>
    v .: "identity"
  parseJSON _ = fail "Expected ApiGatewayRequestContext to be an object."

data ApiGatewayRequestContextIdentity = ApiGatewayRequestContextIdentity
  { apiGatewayRequestContextIdentityCognitoIdentityPoolId         :: !(Maybe Text)
  , apiGatewayRequestContextIdentityAccountId                     :: !(Maybe Text)
  , apiGatewayRequestContextIdentityCognitoIdentityId             :: !(Maybe Text)
  , apiGatewayRequestContextIdentityCaller                        :: !(Maybe Text)
  , apiGatewayRequestContextIdentitySourceIp                      :: !(Maybe Text)
  , apiGatewayRequestContextIdentityPrincipalOrgId                :: !(Maybe Text)
  , apiGatewayRequestContextIdentityAccesskey                     :: !(Maybe Text)
  , apiGatewayRequestContextIdentityCognitoAuthenticationType     :: !(Maybe Text)
  , apiGatewayRequestContextIdentityCognitoAuthenticationProvider :: !(Maybe Value)
  , apiGatewayRequestContextIdentityUserArn                       :: !(Maybe Text)
  , apiGatewayRequestContextIdentityUserAgent                     :: !(Maybe Text)
  , apiGatewayRequestContextIdentityUser                          :: !(Maybe Text)
  } deriving (Show)

instance FromJSON ApiGatewayRequestContextIdentity where
  parseJSON (Object v) = ApiGatewayRequestContextIdentity <$>
    v .: "cognitoIdentityPoolId" <*>
    v .: "accountId" <*>
    v .: "cognitoIdentityId" <*>
    v .: "caller" <*>
    v .: "sourceIp" <*>
    v .: "principalOrgId" <*>
    v .: "accessKey" <*>
    v .: "cognitoAuthenticationType" <*>
    v .: "cognitoAuthenticationProvider" <*>
    v .: "userArn" <*>
    v .: "userAgent" <*>
    v .: "user"
  parseJSON _ = fail "Expected ApiGatewayRequestContextIdentity to be an object."

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