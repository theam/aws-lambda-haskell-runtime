{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}
module Aws.Lambda.Configuration
  ( Main.LambdaOptions(..)
  , Main.generate
  , Main.getRecord
  , configureLambda
  , IPC.returnAndFail
  , IPC.returnAndSucceed
  , Dispatch.decodeObj
  )
where

import qualified Language.Haskell.TH as Meta

import qualified Aws.Lambda.Meta.Main as Main
import qualified Aws.Lambda.Meta.Run as Run
import qualified Aws.Lambda.Meta.Dispatch as Dispatch
import qualified Aws.Lambda.Runtime.IPC as IPC

configureLambda :: Meta.DecsQ
configureLambda = do
  main <- Main.generate
  run <- Run.generate
  return $ main <> [run]
