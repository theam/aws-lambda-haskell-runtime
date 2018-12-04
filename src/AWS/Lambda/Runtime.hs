module AWS.Lambda.Runtime where

import Control.Exception (IOException, try)
import Control.Monad.Except (throwError)
import Data.Aeson
import Relude hiding (get, identity)
import System.Exit (ExitCode (..))

import qualified Data.CaseInsensitive as CI
import Lens.Micro.Platform
import qualified Network.Wreq as Wreq
import qualified System.Environment as Environment
import qualified System.Process as Process


type App a =
  ExceptT RuntimeError IO a


data RuntimeError
  = EnvironmentVariableNotSet Text
  | ApiConnectionError
  | ApiHeaderNotSet Text
  | ParseError Text Text
  | InvocationError Text
  | ResultPublishingError
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


newtype LambdaResult =
  LambdaResult Text


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
    Just value -> pure value
    Nothing    -> throwError (ParseError envVar memoryValue)


getApiData :: Text -> App (Wreq.Response LByteString)
getApiData endpoint =
  tryIO (Wreq.get nextInvocationEndpoint)
 where
  nextInvocationEndpoint :: String
  nextInvocationEndpoint =
    "http://" <> toString endpoint <> "/2018-06-01/runtime/invocation/next"

  tryIO :: IO a -> App a
  tryIO f =
    try f
    & catchApiException

  catchApiException :: IO (Either IOException a) -> App a
  catchApiException action =
    action
    & fmap (first $ const ApiConnectionError)
    & ExceptT


extractHeader :: Wreq.Response LByteString -> Text -> Text
extractHeader apiData header =
  decodeUtf8 (apiData ^. Wreq.responseHeader (CI.mk $ encodeUtf8 header))


extractIntHeader :: Wreq.Response LByteString -> Text -> App Int
extractIntHeader apiData headerName = do
  let header = extractHeader apiData headerName
  case readMaybe $ toString header of
    Nothing    -> throwError (ParseError "deadline" header)
    Just value -> pure value


extractBody :: Wreq.Response LByteString -> Text
extractBody apiData =
  decodeUtf8 (apiData ^. Wreq.responseBody)


propagateXRayTrace :: Text -> App ()
propagateXRayTrace xrayTraceId =
  liftIO $ Environment.setEnv "_X_AMZN_TRACE_ID" $ toString xrayTraceId


initializeContext :: Wreq.Response LByteString -> App Context
initializeContext apiData = do
  functionName          <- readEnvironmentVariable "AWS_LAMBDA_FUNCTION_NAME"
  version               <- readEnvironmentVariable "AWS_LAMBDA_FUNCTION_VERSION"
  logStream             <- readEnvironmentVariable "AWS_LAMBDA_LOG_STREAM_NAME"
  logGroup              <- readEnvironmentVariable "AWS_LAMBDA_LOG_GROUP_NAME"
  memoryLimitInMb       <- readFunctionMemory
  deadline              <- extractIntHeader apiData "Lambda-Runtime-Deadline-Ms"
  let xrayTraceId        = extractHeader apiData "Lambda-Runtime-Trace-Id"
  let awsRequestId       = extractHeader apiData "Lambda-Runtime-Aws-Request-Id"
  let invokedFunctionArn = extractHeader apiData "Lambda-Runtime-Invoked-Function-Arn"
  propagateXRayTrace xrayTraceId
  pure $ Context
    { functionName       = functionName
    , functionVersion    = version
    , logStreamName      = logStream
    , logGroupName       = logGroup
    , memoryLimitInMb    = memoryLimitInMb
    , invokedFunctionArn = invokedFunctionArn
    , xrayTraceId        = xrayTraceId
    , awsRequestId       = awsRequestId
    , deadline           = deadline
    }


invoke :: Text -> Context -> App LambdaResult
invoke event context = do
  let contextJSON = decodeUtf8 $ encode context
  out <- liftIO (Process.readProcessWithExitCode "haskell_lambda" [ toString event, contextJSON ] "")
  case out of
    (ExitSuccess, stdOut, _) -> pure (LambdaResult $ toText stdOut)
    (_, _, stdErr)           -> throwError (InvocationError $ toText stdErr)


publishResult :: Context -> Text -> LambdaResult -> App ()
publishResult Context {..} lambdaApi (LambdaResult result) = do
  let endpoint = "http://"<> lambdaApi <> "/2018-06-01/runtime/invocation/"<> awsRequestId <> "/response"
  void $ liftIO $ Wreq.post (toString endpoint) (toJSON result)


lambda :: App ()
lambda = do
  lambdaApiEndpoint     <- readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"
  apiData               <- getApiData lambdaApiEndpoint
  let event = extractBody apiData
  ctx <- initializeContext apiData
  res <- invoke event ctx
  publishResult ctx lambdaApiEndpoint res


publishError :: RuntimeError -> IO ()
publishError err =
  die (show err)


main :: IO ()
main = do
  res <- runExceptT lambda
  case res of
    Right _  -> exitSuccess
    Left err -> publishError err
