module Aws.Lambda.Runtime
  ( runLambda
  , Runtime.Mode(..)
  , Runtime.LambdaResult(..)
  ) where

import Control.Exception.Safe.Checked
import Control.Monad (forever)
import qualified Network.HTTP.Client as Http

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.IPC as IPC
import qualified Aws.Lambda.Runtime.Publish as Publish
import qualified Aws.Lambda.Runtime.Common as Runtime

-- | Runs the user @haskell_lambda@ executable and posts back the
-- results. This is called from the layer's @main@ function.
runLambda :: Runtime.Mode -> IO ()
runLambda mode = do
  manager <- Http.newManager httpManagerSettings
  forever $ do
    lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
    event     <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing
    context   <- Context.initialize event `catch` errorParsing `catch` variableNotSet
    ((invokeAndRun mode manager lambdaApi event context
      `catch` \err -> Publish.parsingError err lambdaApi context manager)
      `catch` \err -> Publish.invocationError err lambdaApi context manager)
      `catch` \(err :: Error.EnvironmentVariableNotSet) -> Publish.runtimeInitError err lambdaApi context manager

httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
  { Http.managerResponseTimeout = Http.responseTimeoutNone
  }

invokeAndRun
  :: Throws Error.Parsing
  => Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Runtime.Mode
  -> Http.Manager
  -> String
  -> ApiInfo.Event
  -> Context.Context
  -> IO ()
invokeAndRun mode manager lambdaApi event context = do
  result    <- invokeWithMode mode event context
  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

invokeWithMode
  :: Throws Error.Invocation
  => Throws Error.Parsing
  => Throws Error.EnvironmentVariableNotSet
  => Runtime.Mode
  -> ApiInfo.Event
  -> Context.Context
  -> IO Runtime.LambdaResult
invokeWithMode mode event context =
  case mode of
    Runtime.IPC -> IPC.invoke (ApiInfo.event event) context
    (Runtime.DirectCall f) -> do
      handlerName <- Environment.handlerName
      let lambdaOptions = Runtime.LambdaOptions
                          { eventObject = LazyByteString.unpack $ ApiInfo.event event
                          , contextObject = LazyByteString.unpack . encode $ context
                          , functionHandler = handlerName
                          , executionUuid = ""  -- DirectCall doesnt use UUID
                          }
      result <- f lambdaOptions
      case result of
        Left err ->
          throw $ Error.Invocation err
        Right value ->
          pure value

variableNotSet :: Error.EnvironmentVariableNotSet -> IO a
variableNotSet (Error.EnvironmentVariableNotSet env) =
  error ("Error initializing, variable not set: " <> env)

errorParsing :: Error.Parsing -> IO a
errorParsing Error.Parsing{..} =
  error ("Failed parsing " <> errorMessage <> ", got" <> actualValue)
