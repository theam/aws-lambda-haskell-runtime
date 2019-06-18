-- | Main entry point for the layer
module Main
  ( main
  ) where

import Aws.Lambda.Runtime

main :: IO ()
main = runLambda IPC
