module Aws.Lambda.Meta.Run
  ( generate
  ) where

import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Meta.Discover as Discover
import qualified Aws.Lambda.Meta.Dispatch as Dispatch

{-| Generate the run function

It will create a dispatcher that is a huge @case@ expression that
expects the name of the handler provided by AWS Lambda, and will
execute the appropriate user function
 -}
generate :: Meta.DecQ
generate = do
  handlers <- Meta.runIO Discover.handlers
  clause' <- getFieldsFrom "LambdaOptions" ["functionHandler", "contextObject", "eventObject", "executionUuid"]
  body <- Dispatch.generate handlers
  pure $ Meta.FunD (Meta.mkName "run") [Meta.Clause [clause'] (Meta.NormalB body) []]
