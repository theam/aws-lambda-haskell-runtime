{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}
module Aws.Lambda.Configuration
  ( Main.LambdaOptions(..)
  , Main.getRecord
  , configureLambda
  , bootstrapLambda
  , IPC.returnAndFail
  , IPC.returnAndSucceed
  , Dispatch.decodeObj
  , DispatchNoIPC.encodeObj
  )
where

import qualified Language.Haskell.TH as Meta

import qualified Aws.Lambda.Meta.Dispatch as Dispatch
import qualified Aws.Lambda.Meta.DispatchNoIPC as DispatchNoIPC
import qualified Aws.Lambda.Meta.Main as Main
import qualified Aws.Lambda.Meta.Run as Run
import qualified Aws.Lambda.Runtime.IPC as IPC

{-| Generates a @main@ function to be used with the
AWS Lambda layer.
-}
configureLambda :: Meta.DecsQ
configureLambda = do
  main <- Main.generateIPC
  run <- Run.generate
  return (main <> [run])

{-| -}
bootstrapLambda :: Meta.DecsQ
bootstrapLambda = do
  main <- Main.generateDirectCall
  run <- Run.generateNoIPC
  return (main <> [run])