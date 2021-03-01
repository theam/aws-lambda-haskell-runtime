{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Aws.Lambda.Runtime.StandaloneLambda.Types
  ( StandaloneLambdaResponseBody (..),
    ToStandaloneLambdaResponseBody (..),
  )
where

import Aws.Lambda.Utilities (toJSONText)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Wrapper type for lambda response body
newtype StandaloneLambdaResponseBody = StandaloneLambdaResponseBody {unStandaloneLambdaResponseBody :: Text}
  deriving newtype (ToJSON, FromJSON)

class ToStandaloneLambdaResponseBody a where
  toStandaloneLambdaResponse :: a -> StandaloneLambdaResponseBody

-- We need to special case String and Text to avoid unneeded encoding
-- which results in extra quotes put around plain text responses
instance {-# OVERLAPPING #-} ToStandaloneLambdaResponseBody String where
  toStandaloneLambdaResponse = StandaloneLambdaResponseBody . Text.pack

instance {-# OVERLAPPING #-} ToStandaloneLambdaResponseBody Text where
  toStandaloneLambdaResponse = StandaloneLambdaResponseBody

instance ToJSON a => ToStandaloneLambdaResponseBody a where
  toStandaloneLambdaResponse = StandaloneLambdaResponseBody . toJSONText