module Aws.Lambda.Runtime.Error
  ( EnvironmentVariableNotSet(..)
  , ApiConnection(..)
  , ApiHeaderNotSet(..)
  , Parsing(..)
  , Invocation(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Control.Exception.Safe.Checked

newtype EnvironmentVariableNotSet =
  EnvironmentVariableNotSet String
  deriving (Show, Exception)

instance ToJSON EnvironmentVariableNotSet where
  toJSON (EnvironmentVariableNotSet msg) = object
    [ "errorType" .= ("EnvironmentVariableNotSet" :: String)
    , "errorMessage" .= msg
    ]

data ApiConnection =
  ApiConnection
  deriving (Show, Exception)

instance ToJSON ApiConnection where
  toJSON ApiConnection = object
    [ "errorType" .= ("ApiConnection" :: String)
    , "errorMessage" .= ("Could not connect to API to retrieve AWS Lambda parameters" :: String)
    ]

newtype ApiHeaderNotSet =
  ApiHeaderNotSet String
  deriving (Show, Exception)

instance ToJSON ApiHeaderNotSet where
  toJSON (ApiHeaderNotSet headerName) = object
    [ "errorType" .= ("ApiHeaderNotSet" :: String)
    , "errorMessage" .= headerName
    ]

data Parsing = Parsing
  { errorMessage :: String
  , actualValue :: String
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
