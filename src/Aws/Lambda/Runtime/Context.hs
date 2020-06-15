{-# LANGUAGE ScopedTypeVariables #-}

module Aws.Lambda.Runtime.Context
  ( Context(..)
  , initialize
  ) where

import Control.Exception.Safe.Checked
import Data.IORef
import Data.Aeson

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error

-- | Context that is passed to all the handlers
data Context context = Context
  { memoryLimitInMb    :: !Int
  , functionName       :: !String
  , functionVersion    :: !String
  , invokedFunctionArn :: !String
  , awsRequestId       :: !String
  , xrayTraceId        :: !String
  , logStreamName      :: !String
  , logGroupName       :: !String
  , deadline           :: !Int
  , customContext      :: !(IORef context)
  }

instance ToJSON (Context c) where
  toJSON Context{..} = object
    [ "memoryLimitInMb" .= memoryLimitInMb
    , "functionName" .= functionName
    , "functionVersion" .= functionVersion
    , "invokedFunctionArn" .= invokedFunctionArn
    , "awsRequestId" .= awsRequestId
    , "xrayTraceId" .= xrayTraceId
    , "logStreamName" .= logStreamName
    , "logGroupName" .= logGroupName
    , "deadline" .= deadline ]

-- | Initializes the context out of the environment
initialize
  :: forall context. Throws Error.Parsing
  => Throws Error.EnvironmentVariableNotSet
  => IORef context
  -> ApiInfo.Event
  -> IO (Context context)
initialize customContextRef ApiInfo.Event{..} = do
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
    , customContext      = customContextRef
    }
