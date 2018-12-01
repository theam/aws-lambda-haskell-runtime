module AWS.Lambda.Client where

import           Prelude                        ( Show(..) )
import           Data.Aeson
import           Relude                  hiding ( identity )


runtimeApiVersion :: String
runtimeApiVersion = "2018-06-01"

apiContentType :: String
apiContentType = "application/json"

apiErrorContentType :: String
apiErrorContentType = "application/vnd.aws.lambda.error+json"

runtimeErrorHeader :: String
runtimeErrorHeader = "Lambda-Runtime-Function-Error-Type"

data LambdaHeaders
    = LHRequestId
    | LHFunctionArn
    | LHTraceId
    | LHDeadline
    | LHClientContext
    | LHCognitoIdentity

instance Show LambdaHeaders where
  show LHRequestId = "Lambda-Runtime-Aws-Request-Id"
  show LHFunctionArn = "Lambda-Runtime-Invoked-Function-Arn"
  show LHTraceId = "Lambda-Runtime-Trace-Id"
  show LHDeadline = "Lambda-Runtime-Deadline-Ms"
  show LHClientContext = "Lambda-Runtime-Client-Context"
  show LHCognitoIdentity = "Lambda-Runtime-Cognito-Identity"

data ClientApplication = ClientApplication
  { installationId :: String
  , appTitle :: String
  , appVersionName :: String
  , appVersionCode :: String
  , appPackageName :: String
  } deriving Generic

instance FromJSON ClientApplication
instance ToJSON ClientApplication

data ClientContext = ClientContext
  { client :: ClientApplication
  , custom :: Map String String
  , environment :: Map String String
  } deriving Generic

instance FromJSON ClientContext
instance ToJSON ClientContext

data CognitoIdentity = CognitoIdentity
  { identity_id :: String
  , identity_pool_id :: String
  } deriving Generic

instance FromJSON CognitoIdentity
instance ToJSON CognitoIdentity

data EventContext = EventContext
  { invokedFunctionArn :: String
  , awsRequestId :: String
  , xrayTraceId :: String
  , deadline :: Word
  , clientContext :: Maybe ClientContext
  , identity :: Maybe CognitoIdentity
  }

data RuntimeClient = RuntimeClient
  { runtime :: Runtime
  , endpoint :: String
  }
