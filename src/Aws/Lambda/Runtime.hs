module Aws.Lambda.Runtime where

import Control.Exception (IOException, try)
import Control.Monad.Except (catchError, throwError)
import Data.Aeson
import Relude hiding (get, identity)
import System.Exit (ExitCode (..))

import qualified Data.CaseInsensitive as CI
import Lens.Micro.Platform hiding ((.=))
import qualified Network.Wreq as Wreq
import qualified System.Environment as Environment
import qualified System.Process as Process
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Time.Clock.POSIX as Time
import Network.AWS.CloudWatchLogs.PutLogEvents
import Network.AWS.CloudWatchLogs.DescribeLogStreams
import Network.AWS.CloudWatchLogs.Types
import Control.Monad.Trans.AWS
import Network.AWS.Auth
import System.IO (hFlush)


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

  tryIO :: IO (Wreq.Response LByteString) -> App (Wreq.Response LByteString)
  tryIO f = do
    result <- (liftIO $ try f) :: App (Either IOException (Wreq.Response LByteString))
    case result of
      Right x -> return x
      _ -> tryIO f


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


getFunctionResult :: (Text -> App ()) -> UUID.UUID -> Text -> App (Maybe Text)
getFunctionResult cwPrint u stdOut = do
  let out = stdOut
          & toText
          & lines

  out
   & takeWhile (/= uuid)
   & traverse_ ( \t -> do
    liftIO $ putTextLn t
    liftIO $ hFlush stdout)

  out
   & dropWhile (/= uuid)
   & dropWhile (== uuid)
   & nonEmpty
   & fmap head
   & return
 where
  uuid = toText $ UUID.toString u


toAWSRegion :: Text -> Region
toAWSRegion "us-east-1" = NorthVirginia
toAWSRegion "us-east-2" = Ohio
toAWSRegion "us-west-1" = NorthCalifornia
toAWSRegion "us-west-2" = Oregon
toAWSRegion "ca-central-1" = Montreal
toAWSRegion "ap-northeast-1" = Tokyo
toAWSRegion "ap-northeast-2" = Seoul
toAWSRegion "ap-south-1" = Mumbai
toAWSRegion "ap-southeast-1" = Singapore
toAWSRegion "ap-southeast-2" = Sydney
toAWSRegion "sa-east-1" = SaoPaulo
toAWSRegion "eu-west-1" = Ireland
toAWSRegion "eu-west-2" = London
toAWSRegion "eu-central-1" = Frankfurt
toAWSRegion "us-gov-west-1" = GovCloud
toAWSRegion "fips-us-gov-west-1" = GovCloudFIPS
toAWSRegion "cn-north-1" = Beijing
toAWSRegion _ = error "The impossible happened: AWS provided a wrong region"


invoke :: Text -> Context -> App LambdaResult
invoke event context = do
  handlerName <- readEnvironmentVariable "_HANDLER"
  runningDirectory <- readEnvironmentVariable "LAMBDA_TASK_ROOT"
  region <- toAWSRegion <$> readEnvironmentVariable "AWS_REGION"
  amazonEnvironment <- liftIO $ newEnv Discover
  let cwPrint d = liftIO $ runResourceT . runAWST amazonEnvironment . within region $ do
          streamsDescription <- send $ describeLogStreams (logGroupName context)
          let streams = streamsDescription ^. dlsrsLogStreams
          let currentStream = nonEmpty $ filter (\stream -> (stream ^. lsLogStreamName) == (Just $ logStreamName context)) streams
          case head <$> currentStream of
            Nothing -> error "CloudWatch stream not created"
            Just stream -> do
              let nextToken = stream ^. lsUploadSequenceToken
              timestamp <- liftIO $ Time.getPOSIXTime
              let event = inputLogEvent (round timestamp) d
              let operation = putLogEvents (logGroupName context) (logStreamName context) (event :| [])
                            & pleSequenceToken .~ nextToken
              void $ send operation
  let contextJSON = decodeUtf8 $ encode context
  uuid <- liftIO $ UUID.nextRandom
  out <- liftIO $ Process.readProcessWithExitCode (toString runningDirectory <> "/haskell_lambda")
                [ "--eventObject", toString event
                , "--contextObject", contextJSON
                , "--functionHandler", toString handlerName
                , "--executionUuid", UUID.toString uuid
                ]
                ""
  case out of
    (ExitSuccess, stdOut, _) -> do
      cwPrint "Runtime, OK"
      res <- getFunctionResult cwPrint uuid (toText stdOut)
      case res of
        Nothing -> throwError (ParseError "parsing result" $ toText stdOut)
        Just value -> pure (LambdaResult value)
    (_, stdOut, stdErr)           ->
      if (stdErr /= "")
        then throwError (InvocationError $ toText stdErr)
        else do
          res <- getFunctionResult cwPrint uuid (toText stdOut)
          case res of
            Nothing -> throwError (ParseError "parsing error" $ toText stdOut)
            Just value -> throwError (InvocationError value)


publishResult :: Context -> Text -> LambdaResult -> App ()
publishResult Context {..} lambdaApi (LambdaResult result) = do
  let endpoint = "http://"<> lambdaApi <> "/2018-06-01/runtime/invocation/"<> awsRequestId <> "/response"
  void $ liftIO $ Wreq.post (toString endpoint) (encodeUtf8 @Text @ByteString result)


invokeAndPublish :: Context -> Text -> Text -> App ()
invokeAndPublish ctx event lambdaApiEndpoint = do
  res <- invoke event ctx
  publishResult ctx lambdaApiEndpoint res


publishError :: Context -> Text -> RuntimeError -> App ()
publishError Context {..} lambdaApiEndpoint (InvocationError err) = do
  let endpoint = "http://"<> lambdaApiEndpoint <> "/2018-06-01/runtime/invocation/"<> awsRequestId <> "/error"
  void (liftIO $ Wreq.post (toString endpoint) (encodeUtf8 @Text @ByteString err))

publishError Context {..} lambdaApiEndpoint (ParseError t t2) = do
  let endpoint = "http://"<> lambdaApiEndpoint <> "/2018-06-01/runtime/invocation/"<> awsRequestId <> "/error"
  void (liftIO $ Wreq.post (toString endpoint) (toJSON $ ParseError t t2))

publishError Context {..} lambdaApiEndpoint err = do
  let endpoint = "http://"<> lambdaApiEndpoint <> "/2018-06-01/runtime/init/error"
  void (liftIO $ Wreq.post (toString endpoint) (toJSON err))


lambdaRunner :: App ()
lambdaRunner = do
  lambdaApiEndpoint     <- readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"
  apiData               <- getApiData lambdaApiEndpoint
  let event = extractBody apiData
  ctx <- initializeContext apiData
  invokeAndPublish ctx event lambdaApiEndpoint `catchError` publishError ctx lambdaApiEndpoint
