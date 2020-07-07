module Aws.Lambda.Runtime.Context
  ( Context (..),
    initialize,
    setEventData,
  )
where

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Environment as Environment
import Data.IORef

-- | Context that is passed to all the handlers
data Context context
  = Context
      { memoryLimitInMb :: !Int,
        functionName :: !String,
        functionVersion :: !String,
        invokedFunctionArn :: !String,
        awsRequestId :: !String,
        xrayTraceId :: !String,
        logStreamName :: !String,
        logGroupName :: !String,
        deadline :: !Int,
        customContext :: !(IORef context)
      }

-- | Initializes the context out of the environment
initialize ::
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
