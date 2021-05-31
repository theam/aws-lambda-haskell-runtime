{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Aws.Lambda.Runtime.StandaloneLambda.Types
  ( StandaloneLambdaResponseBody (..),
    ToStandaloneLambdaResponseBody (..),
  )
where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text

-- | Wrapper type for lambda response body
data StandaloneLambdaResponseBody
  = StandaloneLambdaResponseBodyPlain Text
  | StandaloneLambdaResponseBodyJson  LBS.ByteString

class ToStandaloneLambdaResponseBody a where
  toStandaloneLambdaResponse :: a -> StandaloneLambdaResponseBody

-- We need to special case String and Text to avoid unneeded encoding
-- which results in extra quotes put around plain text responses
instance {-# OVERLAPPING #-} ToStandaloneLambdaResponseBody String where
  toStandaloneLambdaResponse = StandaloneLambdaResponseBodyPlain . Text.pack

instance {-# OVERLAPPING #-} ToStandaloneLambdaResponseBody Text where
  toStandaloneLambdaResponse = StandaloneLambdaResponseBodyPlain

instance ToJSON a => ToStandaloneLambdaResponseBody a where
  toStandaloneLambdaResponse = StandaloneLambdaResponseBodyJson . encode
