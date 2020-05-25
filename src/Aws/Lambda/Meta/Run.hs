module Aws.Lambda.Meta.Run
  ( generate
  ) where

import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Meta.Discover as Discover
import qualified Aws.Lambda.Meta.Dispatch as Dispatch
import qualified Aws.Lambda.Meta.Main as Main

generate :: Main.DispatcherOptions -> Main.DispatcherStrategy -> Meta.DecQ
generate options strategy = do
  handlers <- Meta.runIO Discover.handlers
  clause' <- getFieldsFrom "LambdaOptions" ["functionHandler", "contextObject", "eventObject", "executionUuid"]
  body <- Dispatch.generate options strategy handlers
  pure $ Meta.FunD (Meta.mkName "run") [Meta.Clause [clause'] (Meta.NormalB body) []]
