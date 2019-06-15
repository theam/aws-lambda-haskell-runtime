module Main where

import Control.Monad
import Control.Monad.Except
import Aws.Lambda.Runtime


main :: IO ()
main =
  forever runLambda
