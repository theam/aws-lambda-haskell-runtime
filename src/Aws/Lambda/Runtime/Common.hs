module Aws.Lambda.Runtime.Common
  ( Mode(..)
  , LambdaResult(..)
  , LambdaOptions(..)
  ) where

import GHC.Generics (Generic)
import qualified Options.Generic as Options

{-| Mode of calling the user functions.

It can be 'IPC' (inter-process communication), where the
dispatcher will spawn a process with the handlers of the
user. (Used when using the layer)

Or, it can be 'DirectCall', for when the handlers are in
the same process. (The runtime is bootstrapped with the
project).
-}
data Mode
  = IPC
  | DirectCall (LambdaOptions -> IO (Either String LambdaResult))
  -- ^ This horrible signature implies the following

-- | Options that the generated main expects
data LambdaOptions = LambdaOptions
  { eventObject     :: !String
  , contextObject   :: !String
  , functionHandler :: !String
  , executionUuid   :: !String
  } deriving (Generic, Options.ParseRecord)

-- | Wrapper type to handle the result of the user
newtype LambdaResult =
  LambdaResult String