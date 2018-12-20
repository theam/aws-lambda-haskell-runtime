module Aws.Lambda.Runtime where

import Control.Exception (Exception, IOException, try)
import Control.Monad.Except (ExceptT, catchError, throwError)
import Data.Aeson
import System.Exit (ExitCode (..))
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as ByteString
import Control.Monad.Trans
import Text.Read (readMaybe)
import qualified Data.Text.Encoding    as Encoding
import Control.Monad
import Data.Function ((&))
import Data.Maybe (listToMaybe)

import qualified Data.CaseInsensitive as CI
import Lens.Micro.Platform hiding ((.=))
import qualified Network.Wreq as Wreq
import qualified System.Environment as Environment
import qualified System.Process as Process
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.IO (hFlush, stdout)


type LByteString = LazyByteString.ByteString
type ByteString = ByteString.ByteString

type App a =
  ExceptT RuntimeError IO a


data RuntimeError
  = EnvironmentVariableNotSet Text
  | ApiConnectionError
  | ApiHeaderNotSet Text
  | ParseError Text Text
  | InvocationError Text
  deriving (Show)
instance Exception RuntimeError

instance ToJSON RuntimeError where
  toJSON (EnvironmentVariableNotSet msg) = object
    [ "errorType" .= ("EnvironmentVariableNotSet" :: Text)
    , "errorMessage" .= msg
    ]

  toJSON ApiConnectionError = object
    [ "errorType" .= ("ApiConnectionError" :: Text)
    , "errorMessage" .= ("Could not connect to API to retrieve AWS Lambda parameters" :: Text)
    ]

  toJSON (ApiHeaderNotSet headerName) = object
    [ "errorType" .= ("ApiHeaderNotSet" :: Text)
    , "errorMessage" .= headerName
    ]

  toJSON (ParseError objectBeingParsed value) = object
    [ "errorType" .= ("ParseError" :: Text)
    , "errorMessage" .= ("Parse error for " <> objectBeingParsed <> ", could not parse value '" <> value <> "'")
    ]

  -- We return the user error as it is
  toJSON (InvocationError err) = toJSON err



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


awsLambdaVersion :: String
awsLambdaVersion = "2018-06-01"


nextInvocationEndpoint :: Text -> String
nextInvocationEndpoint endpoint =
  "http://" <> Text.unpack endpoint <> "/"<> awsLambdaVersion <>"/runtime/invocation/next"


responseEndpoint :: Text -> Text -> String
responseEndpoint lambdaApi requestId =
  "http://"<> Text.unpack lambdaApi <> "/" <> awsLambdaVersion <> "/runtime/invocation/"<> Text.unpack requestId <> "/response"


invocationErrorEndpoint :: Text -> Text -> String
invocationErrorEndpoint lambdaApi requestId =
  "http://"<> Text.unpack lambdaApi <> "/" <> awsLambdaVersion <> "/runtime/invocation/"<> Text.unpack requestId <> "/error"


runtimeInitErrorEndpoint :: Text -> String
runtimeInitErrorEndpoint lambdaApi =
  "http://"<> Text.unpack lambdaApi <> "/" <> awsLambdaVersion <> "/runtime/init/error"


readEnvironmentVariable :: Text -> App Text
readEnvironmentVariable envVar = do
  v <- lift (Environment.lookupEnv $ Text.unpack envVar)
  case v of
    Nothing    -> throwError (EnvironmentVariableNotSet envVar)
    Just value -> pure (Text.pack value)


readFunctionMemory :: App Int
readFunctionMemory = do
  let envVar = "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  let parseMemory txt = readMaybe (Text.unpack txt)
  memoryValue <- readEnvironmentVariable envVar
  case parseMemory memoryValue of
    Just value -> pure value
    Nothing    -> throwError (ParseError envVar memoryValue)


getApiData :: Text -> App (Wreq.Response LByteString)
getApiData endpoint =
  keepRetrying (Wreq.get $ nextInvocationEndpoint endpoint)
 where
  keepRetrying :: IO (Wreq.Response LByteString) -> App (Wreq.Response LByteString)
  keepRetrying f = do
    result <- (liftIO $ try f) :: App (Either IOException (Wreq.Response LByteString))
    case result of
      Right x -> return x
      _ -> keepRetrying f


extractHeader :: Wreq.Response LByteString -> Text -> Text
extractHeader apiData header =
  Encoding.decodeUtf8 (apiData ^. Wreq.responseHeader (CI.mk $ Encoding.encodeUtf8 header))


extractIntHeader :: Wreq.Response LByteString -> Text -> App Int
extractIntHeader apiData headerName = do
  let header = extractHeader apiData headerName
  case readMaybe $ Text.unpack header of
    Nothing    -> throwError (ParseError "deadline" header)
    Just value -> pure value


extractBody :: Wreq.Response LByteString -> Text
extractBody apiData =
  Encoding.decodeUtf8 $ LazyByteString.toStrict (apiData ^. Wreq.responseBody)


propagateXRayTrace :: Text -> App ()
propagateXRayTrace xrayTraceId =
  liftIO $ Environment.setEnv "_X_AMZN_TRACE_ID" $ Text.unpack xrayTraceId


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


getFunctionResult :: UUID.UUID -> Text -> App (Maybe Text)
getFunctionResult u stdOut = do
  let out = Text.lines stdOut

  out
   & takeWhile (/= uuid)
   & mapM_ ( \t -> do
    liftIO $ putStrLn $ Text.unpack t
    liftIO $ hFlush stdout)

  out
   & dropWhile (/= uuid)
   & dropWhile (== uuid)
   & listToMaybe
   & return
 where
  uuid = Text.pack $ UUID.toString u


invoke :: Text -> Context -> App LambdaResult
invoke event context = do
  handlerName <- readEnvironmentVariable "_HANDLER"
  runningDirectory <- readEnvironmentVariable "LAMBDA_TASK_ROOT"
  let contextJSON = Encoding.decodeUtf8 $ LazyByteString.toStrict $ encode context
  uuid <- liftIO UUID.nextRandom
  out <- liftIO $ Process.readProcessWithExitCode (Text.unpack runningDirectory <> "/haskell_lambda")
                [ "--eventObject", Text.unpack event
                , "--contextObject", Text.unpack contextJSON
                , "--functionHandler", Text.unpack handlerName
                , "--executionUuid", UUID.toString uuid
                ]
                ""
  case out of
    (ExitSuccess, stdOut, _) -> do
      res <- getFunctionResult uuid (Text.pack stdOut)
      case res of
        Nothing -> throwError (ParseError "parsing result" $ Text.pack stdOut)
        Just value -> pure (LambdaResult value)
    (_, stdOut, stdErr)           ->
      if stdErr /= ""
        then throwError (InvocationError $ Text.pack stdErr)
        else do
          res <- getFunctionResult uuid (Text.pack stdOut)
          case res of
            Nothing -> throwError (ParseError "parsing error" $ Text.pack stdOut)
            Just value -> throwError (InvocationError value)


publishResult :: Context -> Text -> LambdaResult -> App ()
publishResult Context {..} lambdaApi (LambdaResult result) =
  void $ liftIO $ Wreq.post (responseEndpoint lambdaApi awsRequestId) (Encoding.encodeUtf8 result)


invokeAndPublish :: Context -> Text -> Text -> App ()
invokeAndPublish ctx event lambdaApiEndpoint = do
  res <- invoke event ctx
  publishResult ctx lambdaApiEndpoint res


publishError :: Context -> Text -> RuntimeError -> App ()
publishError Context {..} lambdaApiEndpoint (InvocationError err) =
  void (liftIO $ Wreq.post (invocationErrorEndpoint lambdaApiEndpoint awsRequestId) (Encoding.encodeUtf8 err))

publishError Context {..} lambdaApiEndpoint (ParseError t t2) =
  void (liftIO $ Wreq.post (invocationErrorEndpoint lambdaApiEndpoint awsRequestId) (toJSON $ ParseError t t2))

publishError Context {..} lambdaApiEndpoint err =
  void (liftIO $ Wreq.post (runtimeInitErrorEndpoint lambdaApiEndpoint) (toJSON err))


lambdaRunner :: App ()
lambdaRunner = do
  lambdaApiEndpoint     <- readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"
  apiData               <- getApiData lambdaApiEndpoint
  let event = extractBody apiData
  ctx <- initializeContext apiData
  invokeAndPublish ctx event lambdaApiEndpoint `catchError` publishError ctx lambdaApiEndpoint
