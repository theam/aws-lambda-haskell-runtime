module Aws.Lambda.Runtime.Context
  ( Context (..),
    initialize,
    setEventData,
  )
where

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import Control.Exception.Safe.Checked (Throws)
import Data.IORef (IORef)
import Data.Text (Text)

-- | Context that is passed to all the handlers
data Context context = Context
  { memoryLimitInMb :: !Int,
    functionName :: !Text,
    functionVersion :: !Text,
    invokedFunctionArn :: !Text,
    awsRequestId :: !Text,
    xrayTraceId :: !Text,
    logStreamName :: !Text,
    logGroupName :: !Text,
    deadline :: !Int,
    customContext :: !(IORef context)
  }

-- | Initializes the context out of the environment
initialize ::
  Throws Error.Parsing =>
  Throws Error.EnvironmentVariableNotSet =>
  IORef context ->
  IO (Context context)
initialize customContextRef = do
  functionName <- Environment.functionName
  version <- Environment.functionVersion
  logStream <- Environment.logStreamName
  logGroup <- Environment.logGroupName
  memoryLimitInMb <- Environment.functionMemory

  pure $
    Context
      { functionName = functionName,
        functionVersion = version,
        logStreamName = logStream,
        logGroupName = logGroup,
        memoryLimitInMb = memoryLimitInMb,
        customContext = customContextRef,
        -- We set those to "empty" values because they will be assigned
        -- from the incoming event once one has been received. (see setEventData)
        invokedFunctionArn = mempty,
        xrayTraceId = mempty,
        awsRequestId = mempty,
        deadline = 0
      }

-- | Sets the context's event data
setEventData ::
  Context context ->
  ApiInfo.Event ->
  IO (Context context)
setEventData context ApiInfo.Event {..} = do
  Environment.setXRayTrace traceId

  return $
    context
      { invokedFunctionArn = invokedFunctionArn,
        xrayTraceId = traceId,
        awsRequestId = awsRequestId,
        deadline = deadlineMs
      }