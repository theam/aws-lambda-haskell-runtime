module Aws.Lambda.Runtime.Result
  ( LambdaResult(..)
  ) where

import Data.Aeson

newtype LambdaResult =
  LambdaResult String

instance ToJSON LambdaResult where
  toJSON (LambdaResult result) =
    toJSON result
