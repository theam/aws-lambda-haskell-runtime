module Aws.Lambda.Runtime.Error
  ( EnvironmentVariableNotSet(..)
  , Parsing(..)
  , Invocation(..)
  ) where

import Control.Exception.Safe.Checked
import Data.Aeson (ToJSON (..), object, (.=))

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
  } deriving (Show, Exception)

instance ToJSON Parsing where
  toJSON (Parsing objectBeingParsed value) = object
    [ "errorType" .= ("Parsing" :: String)
    , "errorMessage" .= ("Parse error for " <> objectBeingParsed <> ", could not parse value '" <> value <> "'")
    ]

newtype Invocation =
  Invocation String
  deriving (Show, Exception)

instance ToJSON Invocation where
  -- We return the user error as it is
  toJSON (Invocation err) = toJSON err
