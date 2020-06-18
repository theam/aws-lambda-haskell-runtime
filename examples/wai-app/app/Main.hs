{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import Aws.Lambda
import Lib
import qualified Lib

-- This tells the Lambda runtime how to initialize your application context.
-- If you do not wish to use a shared context, you can just use Unit as the context value.
-- E.g.
-- initializeContext :: IO ()
-- initializeContext = return ()
initializeContext :: IO AppConfig
initializeContext = Lib.initializeAppConfig

generateLambdaDispatcher UseWithAPIGateway defaultDispatcherOptions
