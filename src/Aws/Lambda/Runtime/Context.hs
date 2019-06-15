module Aws.Lambda.Runtime.Context
  ( Context(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

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
  } deriving (Generic, FromJSON, ToJSON)
