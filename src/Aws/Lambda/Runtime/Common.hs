module Aws.Lambda.Runtime.Common
  ( RunCallback
  , LambdaResult(..)
  , LambdaOptions(..)
  ) where

import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Aws.Lambda.Runtime.Context (Context)

-- | Callback that we pass to the dispatcher function
type RunCallback =
  LambdaOptions -> IO (Either ByteString LambdaResult)

-- | Options that the generated main expects
data LambdaOptions = LambdaOptions
  { eventObject     :: !ByteString
  , contextObject   :: !Context
  , functionHandler :: !String
  , executionUuid   :: !String
  } deriving (Generic)

-- | Wrapper type to handle the result of the user
newtype LambdaResult =
  LambdaResult ByteString
