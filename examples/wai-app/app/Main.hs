{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Aws.Lambda
import qualified Lib

initializeContext :: IO ()
initializeContext = pure ()

generateLambdaDispatcher UseWithAPIGateway defaultDispatcherOptions
