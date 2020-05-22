{-| All the errors that the runtime can throw
-}
module Aws.Lambda.Runtime.Error
  ( EnvironmentVariableNotSet(..)
  , Parsing(..)
  , Invocation(..)
  ) where

import Control.Exception.Safe.Checked
import Data.Aeson (ToJSON (..), object, (.=), Value)

newtype EnvironmentVariableNotSet =
  EnvironmentVariableNotSet String
  deriving (Show, Exception)

instance ToJSON EnvironmentVariableNotSet where
  toJSON (EnvironmentVariableNotSet msg) = object
    [ "errorType" .= ("EnvironmentVariableNotSet" :: String)
    , "errorMessage" .= msg
    ]

data Parsing = Parsing
  { errorMessage :: String
  , actualValue  :: String
  , valueName    :: String
  } deriving (Show, Exception)

instance ToJSON Parsing where
  toJSON (Parsing errorMessage value _) = object
    [ "errorType" .= ("Parsing" :: String)
    , "errorMessage" .= ("Could not parse value '" <> value <> "': " <> errorMessage)
    ]

newtype Invocation =
  Invocation Value
  deriving (Show, Exception)

instance ToJSON Invocation where
  -- We return the user error as it is
  toJSON (Invocation err) = err
