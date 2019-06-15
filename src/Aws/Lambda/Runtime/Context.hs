module Aws.Lambda.Runtime.Context
  ( Context(..)
  , initialize
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Control.Exception.Safe.Checked

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error

data Context = Context
  { memoryLimitInMb    :: !Int
  , functionName       :: !String
  , functionVersion    :: !String
  , invokedFunctionArn :: !String
  , awsRequestId       :: !String
  , xrayTraceId        :: !String
  , logStreamName      :: !String
  , logGroupName       :: !String
  , deadline           :: !Int
  } deriving (Generic, FromJSON, ToJSON)

initialize :: Throws Error.Parsing => Throws Error.EnvironmentVariableNotSet => ApiInfo.Event -> IO Context
initialize ApiInfo.Event{..} = do
  functionName          <- Environment.functionName
  version               <- Environment.functionVersion
  logStream             <- Environment.logStreamName
  logGroup              <- Environment.logGroupName
  memoryLimitInMb       <- Environment.functionMemory
  Environment.setXRayTrace traceId
  pure Context
    { functionName       = functionName
    , functionVersion    = version
    , logStreamName      = logStream
    , logGroupName       = logGroup
    , memoryLimitInMb    = memoryLimitInMb
    , invokedFunctionArn = invokedFunctionArn
    , xrayTraceId        = traceId
    , awsRequestId       = awsRequestId
    , deadline           = deadlineMs
    }