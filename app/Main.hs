module Main where

import Relude
import Aws.Lambda.Runtime


main :: IO ()
main = do
  res <- runExceptT lambdaRunner
  case res of
    Right _ -> exitSuccess
    Left _  -> exitFailure
