-- | All the errors that the runtime can throw
module Aws.Lambda.Runtime.Error
  ( EnvironmentVariableNotSet (..),
    Parsing (..),
    Invocation (..),
  )
where

import Control.Exception
import Data.Aeson ((.=), ToJSON (..), Value, object)

newtype EnvironmentVariableNotSet
  = EnvironmentVariableNotSet String
  deriving (Show, Exception)

instance ToJSON EnvironmentVariableNotSet where
  toJSON (EnvironmentVariableNotSet msg) =
    object
      [ "errorType" .= ("EnvironmentVariableNotSet" :: String),
        "errorMessage" .= msg
      ]

data Parsing
  = Parsing
      { errorMessage :: String,
        actualValue :: String,
        valueName :: String
      }
  deriving (Show, Exception)

instance ToJSON Parsing where
  toJSON (Parsing errorMessage _ valueName) =
    object
      [ "errorType" .= ("Parsing" :: String),
        "errorMessage" .= ("Could not parse '" <> valueName <> "': " <> errorMessage)
      ]

newtype Invocation
  = Invocation Value
  deriving (Show, Exception)

instance ToJSON Invocation where
  -- We return the user error as it is
  toJSON (Invocation err) = err
