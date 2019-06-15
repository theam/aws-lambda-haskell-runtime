module Aws.Lambda.Runtime
  ( runLambda
  ) where

import qualified Network.HTTP.Client as Http


import Control.Exception.Safe.Checked

import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.IPC as IPC
import qualified Aws.Lambda.Runtime.Publish as Publish


httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
  { Http.managerResponseTimeout = Http.responseTimeoutNone
  }

runLambda
  :: Throws Error.EnvironmentVariableNotSet
  => Throws Error.Parsing
  => Throws Error.Invocation
  => IO ()
runLambda = do
  manager <- Http.newManager httpManagerSettings
  lambdaApi     <- Environment.apiEndpoint
  event            <- ApiInfo.fetchEvent manager lambdaApi
  context <- Context.initialize event
  result <- IPC.invoke (ApiInfo.event event) context
  Publish.result result lambdaApi context manager
