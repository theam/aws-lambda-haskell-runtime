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
import Control.Exception.Safe (throw)
import Data.Text (Text, pack, unpack)
import qualified System.Environment as Environment
import qualified Text.Read as Read

logGroupName :: IO Text
logGroupName =
  readEnvironmentVariable "AWS_LAMBDA_LOG_GROUP_NAME"

logStreamName :: IO Text
logStreamName =
  readEnvironmentVariable "AWS_LAMBDA_LOG_STREAM_NAME"

functionVersion :: IO Text
functionVersion =
  readEnvironmentVariable "AWS_LAMBDA_FUNCTION_VERSION"

functionName :: IO Text
functionName =
  readEnvironmentVariable "AWS_LAMBDA_FUNCTION_NAME"

setXRayTrace :: Text -> IO ()
setXRayTrace = Environment.setEnv "_X_AMZN_TRACE_ID" . unpack

taskRoot :: IO Text
taskRoot =
  readEnvironmentVariable "LAMBDA_TASK_ROOT"

handlerName :: IO Text
handlerName =
  readEnvironmentVariable "_HANDLER"

apiEndpoint :: IO Text
apiEndpoint =
  readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"

functionMemory :: IO Int
functionMemory = do
  let envVar = "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  memoryValue <- readEnvironmentVariable envVar
  case Read.readMaybe (unpack memoryValue) of
    Just value -> pure value
    Nothing -> throw (Error.Parsing envVar memoryValue envVar)

readEnvironmentVariable :: Text -> IO Text
readEnvironmentVariable envVar = do
  v <- Environment.lookupEnv (unpack envVar)
  case v of
    Just value -> pure . pack $ value
    Nothing -> throw (Error.EnvironmentVariableNotSet envVar)
