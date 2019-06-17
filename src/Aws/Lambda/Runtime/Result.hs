module Aws.Lambda.Runtime.Result
  ( LambdaResult(..)
  ) where

-- | Wrapper type to handle the result of the user
newtype LambdaResult =
  LambdaResult String
