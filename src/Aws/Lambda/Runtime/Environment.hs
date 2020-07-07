-- | Provides all the values out of
-- the environment variables of the system
module Aws.Lambda.Runtime.Environment
  ( functionMemory,
    apiEndpoint,
    handlerName,
    taskRoot,
    functionName,
    functionVersion,
    logStreamName,
    logGroupName,
    setXRayTrace,
  )
where

import qualified Aws.Lambda.Runtime.Error as Error
import Control.Exception (throw)
import qualified System.Environment as Environment
import qualified Text.Read as Read

logGroupName :: IO String
logGroupName =
  readEnvironmentVariable "AWS_LAMBDA_LOG_GROUP_NAME"

logStreamName :: IO String
logStreamName =
  readEnvironmentVariable "AWS_LAMBDA_LOG_STREAM_NAME"

functionVersion :: IO String
functionVersion =
  readEnvironmentVariable "AWS_LAMBDA_FUNCTION_VERSION"

functionName :: IO String
functionName =
  readEnvironmentVariable "AWS_LAMBDA_FUNCTION_NAME"

setXRayTrace :: String -> IO ()
setXRayTrace = Environment.setEnv "_X_AMZN_TRACE_ID"

taskRoot :: IO String
taskRoot =
  readEnvironmentVariable "LAMBDA_TASK_ROOT"

handlerName :: IO String
handlerName =
  readEnvironmentVariable "_HANDLER"

apiEndpoint :: IO String
apiEndpoint =
  readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"

functionMemory :: IO Int
functionMemory = do
  let envVar = "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  memoryValue <- readEnvironmentVariable envVar
  case Read.readMaybe memoryValue of
    Just value -> pure value
    Nothing -> throw (Error.Parsing envVar memoryValue envVar)

readEnvironmentVariable :: String -> IO String
readEnvironmentVariable envVar = do
  v <- Environment.lookupEnv envVar
  case v of
    Just value -> pure value
    Nothing -> throw (Error.EnvironmentVariableNotSet envVar)
