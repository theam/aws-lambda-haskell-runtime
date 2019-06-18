{-| main function generation for interoperation with the layer -}
module Aws.Lambda.Meta.Main
  ( Runtime.LambdaOptions(..)
  , generateIPC
  , generateDirectCall
  , Options.getRecord
  ) where

import qualified Language.Haskell.TH as Meta
import qualified Options.Generic as Options

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Runtime.Common as Runtime

-- | Generate the main function that the layer will call
generateIPC :: Meta.DecsQ
generateIPC = [d|
  $(declarationName "main") = getRecord "" >>= run
  |]

generateDirectCall :: Meta.DecsQ
generateDirectCall = [d|
  $(declarationName "main") = $(directCallBody)
  |]
 where
  directCallBody =
    [e|do
    runLambda $ $(constructorName "DirectCall") run
    |]
