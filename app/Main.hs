module Main where

import Relude
import Aws.Lambda.Runtime


main :: IO ()
main = forever $ do
  res <- runExceptT lambdaRunner
  case res of
    Right _ -> return ()
    Left err  -> putTextLn $ show err
