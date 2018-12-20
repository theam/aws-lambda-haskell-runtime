module Main where

import Control.Monad
import Control.Monad.Except
import Aws.Lambda.Runtime


main :: IO ()
main = forever $ do
  res <- runExceptT lambdaRunner
  case res of
    Right _ -> return ()
    Left err  -> putStrLn $ show err
