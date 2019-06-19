module Aws.Lambda.Runtime.Common
  ( RunCallback
  , LambdaResult(..)
  , LambdaOptions(..)
  ) where

import GHC.Generics (Generic)

-- | Callback that we pass to the dispatcher function
type RunCallback =
  LambdaOptions -> IO (Either String LambdaResult)

-- | Options that the generated main expects
data LambdaOptions = LambdaOptions
  { eventObject     :: !String
  , contextObject   :: !String
  , functionHandler :: !String
  , executionUuid   :: !String
  } deriving (Generic)

-- | Wrapper type to handle the result of the user
newtype LambdaResult =
  LambdaResult String