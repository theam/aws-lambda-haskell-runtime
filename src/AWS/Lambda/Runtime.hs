module AWS.Lambda.Runtime where

import Relude hiding (get, identity)

import Control.Exception (IOException, try)
import Control.Monad.Except (throwError)
import Data.Aeson
import Lens.Micro.Platform
import qualified Network.Wreq as Wreq
import qualified System.Environment as Environment


type App a = ExceptT RuntimeError IO a

data RuntimeError
  = EnvironmentVariableNotSet Text
  | ApiConnectionError
  | ApiHeaderNotSet Text
  | ParseError Text
  | OtherError Text
  deriving (Show)
instance Exception RuntimeError


data Context = Context
  { memoryLimitInMb    :: !Int
  , functionName       :: !Text
  , functionVersion    :: !Text
  , invokedFunctionArn :: !Text
  , awsRequestId       :: !Text
  , xrayTraceId        :: !Text
  , logStreamName      :: !Text
  , logGroupName       :: !Text
  , deadline           :: !Int
  } deriving (Generic)

instance FromJSON Context
instance ToJSON Context

readEnvironmentVariable :: Text -> App Text
readEnvironmentVariable envVar = do
  v <- lift (Environment.lookupEnv $ toString envVar)
  case v of
    Nothing    -> throwError (EnvironmentVariableNotSet envVar)
    Just value -> pure (toText value)

readFunctionMemory :: App Int
readFunctionMemory = do
  let envVar = "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  let parseMemory txt = readMaybe (toString txt)
  memoryValue <- readEnvironmentVariable envVar
  case parseMemory memoryValue of
    Just (value :: Int) -> pure value
    Nothing ->
      throwError
        $  ParseError
        $  "Could not parse memory size from environment variable "
        <> envVar
        <> " - value was "
        <> memoryValue

getApiData :: Text -> App (Wreq.Response LByteString)
getApiData endpoint = do
  apiData <- tryIO $ Wreq.get
    ("http://" <> toString endpoint <> "/2018-06-01/runtime/invocation/next")
  -- first (const ApiConnectionError) apiData
  pure apiData
 where
  tryIO :: IO a -> App a
  tryIO f =
    try f
    & catchApiException

  catchApiException :: IO (Either IOException a) -> App a
  catchApiException action =
    action
    & fmap (first $ const ApiConnectionError)
    & ExceptT

initializeContext :: App Context
initializeContext = do
  functionName      <- readEnvironmentVariable "AWS_LAMBDA_FUNCTION_NAME"
  version           <- readEnvironmentVariable "AWS_LAMBDA_FUNCTION_VERSION"
  logStream         <- readEnvironmentVariable "AWS_LAMBDA_LOG_STREAM_NAME"
  logGroup          <- readEnvironmentVariable "AWS_LAMBDA_LOG_GROUP_NAME"
  lambdaApiEndpoint <- readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"
  memoryLimitInMb   <- readFunctionMemory
  apiData           <- getApiData lambdaApiEndpoint
  let xrayTraceId :: Text =
        decodeUtf8 $ apiData ^. Wreq.responseHeader "Lambda-Runtime-Trace-Id"
  let awsRequestId :: Text =
        decodeUtf8 $ apiData ^. Wreq.responseHeader "Lambda-Runtime-Aws-Request-Id"
  liftIO $ Environment.setEnv "_X_AMZN_TRACE_ID" $ toString xrayTraceId
  let invokedFunctionArn :: Text = decodeUtf8 $ apiData ^. Wreq.responseHeader
        "Lambda-Runtime-Invoked-Function-Arn"
  let deadline :: Maybe Int =
        readMaybe $ decodeUtf8 $ apiData ^. Wreq.responseHeader
          "Lambda-Runtime-Deadline-Ms"

  putTextLn (decodeUtf8 $ apiData ^. Wreq.responseHeader "")
  pure $ Context
    { functionName       = functionName
    , functionVersion    = version
    , logStreamName      = logStream
    , logGroupName       = logGroup
    , memoryLimitInMb    = memoryLimitInMb
    , invokedFunctionArn = invokedFunctionArn
    , xrayTraceId        = xrayTraceId
    , awsRequestId       = awsRequestId
    , deadline           = deadline ?: error "Could not parse deadline"
    }

lambda
  :: (FromJSON input, ToJSON output)
  => (input -> Context -> IO (Either Text output))
  -> IO ()
lambda handler = do
  ctx <- runExceptT initializeContext
  res <- handler undefined (fromRight (error "AAAAAAA") ctx)
  either print (print . encode) res