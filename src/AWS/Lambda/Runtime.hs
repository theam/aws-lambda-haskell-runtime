module AWS.Lambda.Runtime where

import           Relude                  hiding ( identity
                                                , get
                                                )

import           Data.Aeson
import           Data.Time.Clock.POSIX
import           System.Environment
import           Network.Wreq
import           Lens.Micro.Platform


data Context = Context
  { memoryLimitInMb :: Int
  , functionName :: String
  , functionVersion :: String
  , invokedFunctionArn :: String
  , awsRequestId :: String
  , xrayTraceId :: String
  , logStreamName :: String
  , logGroupName :: String
  , clientContext :: Maybe ClientContext
  , identity :: Maybe CognitoIdentity
  , deadline :: Int
  }

initializeContext :: IO (Either String Context)
initializeContext = do
  functionName        <- lookupEnv "AWS_LAMBDA_FUNCTION_NAME"
  version             <- lookupEnv "AWS_LAMBDA_FUNCTION_VERSION"
  logStream           <- lookupEnv "AWS_LAMBDA_LOG_STREAM_NAME"
  logGroup            <- lookupEnv "AWS_LAMBDA_LOG_GROUP_NAME"
  memoryStr           <- lookupEnv "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  awsLambdaRuntimeApi <- getRuntimeApiEndpoint
  apiData             <- get
    (  "http://"
    <> fromRight "" awsLambdaRuntimeApi
    <> "/2018-06-01/runtime/invocation/next"
    )
  let xrayTraceId :: String =
        decodeUtf8 $ apiData ^. responseHeader "Lambda-Runtime-Trace-Id"
  let awsRequestId :: String =
        decodeUtf8 $ apiData ^. responseHeader "Lambda-Runtime-Aws-Request-Id"
  setEnv "_X_AMZN_TRACE_ID" xrayTraceId
  let invokedFunctionArn :: String = decodeUtf8 $ apiData ^. responseHeader
        "Lambda-Runtime-Invoked-Function-Arn"
  let deadline :: Maybe Int =
        readMaybe $ decodeUtf8 $ apiData ^. responseHeader
          "Lambda-Runtime-Deadline-Ms"

  putTextLn (decodeUtf8 $ apiData ^. responseHeader "")
  let parsedMemory = memoryStr >>= readMaybe
  case parsedMemory of
    Nothing -> do
      let err =
            "Could not parse memory value: "
              <> (memoryStr ?: "<NOTHING>")
              <> "\nMemory value from environment is not an 'Int'"
      return $ Left err

    Just (mem :: Int) -> return $ Right $ Context
      { functionName       = functionName ?: ""
      , functionVersion    = version ?: ""
      , logStreamName      = logStream ?: ""
      , logGroupName       = logGroup ?: ""
      , memoryLimitInMb    = mem
      , invokedFunctionArn = invokedFunctionArn
      , xrayTraceId        = xrayTraceId
      , awsRequestId       = awsRequestId
      , clientContext      = Nothing
      , identity           = Nothing
      , deadline           = deadline ?: error "Could not parse deadline"
      }


getTimeRemainingMillis :: Context -> IO Int
getTimeRemainingMillis Context {..} = do
  millis <- getPOSIXTime
  return (fromIntegral deadline - round millis)

getRuntimeApiEndpoint :: IO (Either String String)
getRuntimeApiEndpoint = do
  endpoint <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
  case endpoint of
    Nothing -> do
      let err = "Could not read endpoint, was it set?"
      return $ Left err
    Just ep -> return $ Right ep

lambda
  :: (FromJSON input, ToJSON output)
  => (input -> Context -> IO (Either String output))
  -> IO ()
lambda _ = do
  api <- getRuntimeApiEndpoint
  case api of
    Right awsLambdaRuntimeApi -> do
      putTextLn "hi"
      putTextLn "hi"
    Left err -> putTextLn $ toText err

data ClientContext
data CognitoIdentity
