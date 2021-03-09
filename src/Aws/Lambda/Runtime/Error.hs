-- | All the errors that the runtime can throw
module Aws.Lambda.Runtime.Error
  ( EnvironmentVariableNotSet (..),
    Parsing (..),
    Invocation (..),
  )
where

import Control.Exception.Safe.Checked (Exception)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)

newtype EnvironmentVariableNotSet
  = EnvironmentVariableNotSet Text
  deriving (Show, Exception)

instance ToJSON EnvironmentVariableNotSet where
  toJSON (EnvironmentVariableNotSet msg) =
    object
      [ "errorType" .= ("EnvironmentVariableNotSet" :: Text),
        "errorMessage" .= msg
      ]

data Parsing = Parsing
  { errorMessage :: Text,
    actualValue :: Text,
    valueName :: Text
  }
  deriving (Show, Exception)

instance ToJSON Parsing where
  toJSON (Parsing errorMessage _ valueName) =
    object
      [ "errorType" .= ("Parsing" :: Text),
        "errorMessage" .= ("Could not parse '" <> valueName <> "': " <> errorMessage)
      ]

newtype Invocation
  = Invocation LBS.ByteString
  deriving (Show, Exception)
