-- | main function generation for interoperation with the layer
module Aws.Lambda.Meta.Main
  ( Runtime.LambdaOptions (..),
    Runtime.DispatcherStrategy (..),
    Runtime.DispatcherOptions (..),
    Runtime.ApiGatewayDispatcherOptions (..),
    Runtime.defaultDispatcherOptions,
    generate,
  )
where

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Language.Haskell.TH as Meta

-- | Generate the main function with the dispatcher
generate :: Meta.DecsQ
generate =
  [d|
    $(declarationName "main") = $(directCallBody)
    |]  where
    directCallBody =
      [e|
        runLambda initializeContext run
        |]
