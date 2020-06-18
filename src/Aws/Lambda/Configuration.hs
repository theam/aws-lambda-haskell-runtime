{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}
module Aws.Lambda.Configuration
  ( Main.LambdaOptions(..)
  , generateLambdaDispatcher
  , Dispatch.decodeObj
  )
where

import qualified Language.Haskell.TH as Meta

import qualified Aws.Lambda.Meta.Dispatch as Dispatch
import qualified Aws.Lambda.Meta.Main as Main
import qualified Aws.Lambda.Meta.Run as Run

{-| Generates a @main@ function that acts as a dispatcher
-}
generateLambdaDispatcher :: Main.DispatcherStrategy -> Main.DispatcherOptions -> Meta.DecsQ
generateLambdaDispatcher strategy options = do
  main <- Main.generate
  run <- Run.generate options strategy
  return (main <> [run])
