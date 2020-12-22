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
import Control.Exception.Safe.Checked (Throws, throw)
import Data.Text (Text, pack, unpack)
import qualified System.Environment as Environment
import qualified Text.Read as Read

logGroupName :: Throws Error.EnvironmentVariableNotSet => IO Text
logGroupName =
  readEnvironmentVariable "AWS_LAMBDA_LOG_GROUP_NAME"

logStreamName :: Throws Error.EnvironmentVariableNotSet => IO Text
logStreamName =
  readEnvironmentVariable "AWS_LAMBDA_LOG_STREAM_NAME"

functionVersion :: Throws Error.EnvironmentVariableNotSet => IO Text
functionVersion =
  readEnvironmentVariable "AWS_LAMBDA_FUNCTION_VERSION"

functionName :: Throws Error.EnvironmentVariableNotSet => IO Text
functionName =
  readEnvironmentVariable "AWS_LAMBDA_FUNCTION_NAME"

setXRayTrace :: Text -> IO ()
setXRayTrace = Environment.setEnv "_X_AMZN_TRACE_ID" . unpack

taskRoot :: Throws Error.EnvironmentVariableNotSet => IO Text
taskRoot =
  readEnvironmentVariable "LAMBDA_TASK_ROOT"

handlerName :: Throws Error.EnvironmentVariableNotSet => IO Text
handlerName =
  readEnvironmentVariable "_HANDLER"

apiEndpoint :: Throws Error.EnvironmentVariableNotSet => IO Text
apiEndpoint =
  readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"

functionMemory :: Throws Error.Parsing => Throws Error.EnvironmentVariableNotSet => IO Int
functionMemory = do
  let envVar = "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  memoryValue <- readEnvironmentVariable envVar
  case Read.readMaybe (unpack memoryValue) of
    Just value -> pure value
    Nothing -> throw (Error.Parsing envVar memoryValue envVar)

readEnvironmentVariable :: Throws Error.EnvironmentVariableNotSet => Text -> IO Text
readEnvironmentVariable envVar = do
  v <- Environment.lookupEnv (unpack envVar)
  case v of
    Just value -> pure . pack $ value
    Nothing -> throw (Error.EnvironmentVariableNotSet envVar)
