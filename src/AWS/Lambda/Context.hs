module AWS.Lambda.Context where

import           Relude                  hiding ( identity )

import           Data.Time.Clock.POSIX

import           AWS.Lambda.Env


-- TODO: Declare this in its own module
data ClientContext

-- TODO: Declare this in its own module
data CognitoIdentity


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
  , deadline :: Word
  }

makeContext :: FunctionSettings -> Context
makeContext FunctionSettings {..} = Context
  { xrayTraceId        = ""
  , memoryLimitInMb    = memorySize
  , functionName       = functionName
  , functionVersion    = version
  , logStreamName      = logStream
  , logGroupName       = logGroup
  , invokedFunctionArn = ""
  , awsRequestId       = ""
  , clientContext      = Nothing
  , identity           = Nothing
  , deadline           = 0
  }

getTimeRemainingMillis :: Context -> IO Int
getTimeRemainingMillis Context {..} = do
  millis <- getPOSIXTime
  return (fromIntegral deadline - round millis)
