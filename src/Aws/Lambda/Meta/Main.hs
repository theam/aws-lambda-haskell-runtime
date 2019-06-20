{-| main function generation for interoperation with the layer -}
module Aws.Lambda.Meta.Main
  ( Runtime.LambdaOptions(..)
  , generate
  ) where

import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Runtime.Common as Runtime

-- | Generate the main function with the dispatcher
generate :: Meta.DecsQ
generate = [d|
  $(declarationName "main") = $(directCallBody)
  |]
 where
  directCallBody =
    [e|do
    runLambda run
    |]
