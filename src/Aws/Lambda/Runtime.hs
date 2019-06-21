module Aws.Lambda.Runtime
  ( runLambda
  , Runtime.RunCallback
  , Runtime.LambdaResult(..)
  ) where

import Control.Exception.Safe.Checked
import Control.Monad (forever)
import qualified Network.HTTP.Client as Http

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.Publish as Publish

-- | Runs the user @haskell_lambda@ executable and posts back the
-- results. This is called from the layer's @main@ function.
runLambda :: Runtime.RunCallback -> IO ()
runLambda callback = do
  manager <- Http.newManager httpManagerSettings
  forever $ do
    lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
    event     <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing
    context   <- Context.initialize event `catch` errorParsing `catch` variableNotSet
    ((invokeAndRun callback manager lambdaApi event context
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
  :: Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Runtime.RunCallback
  -> Http.Manager
  -> String
  -> ApiInfo.Event
  -> Context.Context
  -> IO ()
invokeAndRun callback manager lambdaApi event context = do
  result    <- invokeWithCallback callback event context
  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

invokeWithCallback
  :: Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Runtime.RunCallback
  -> ApiInfo.Event
  -> Context.Context
  -> IO Runtime.LambdaResult
invokeWithCallback callback event context = do
  handlerName <- Environment.handlerName
  let lambdaOptions = Runtime.LambdaOptions
                      { eventObject = LazyByteString.unpack $ ApiInfo.event event
                      , contextObject = LazyByteString.unpack . encode $ context
                      , functionHandler = handlerName
                      , executionUuid = ""  -- DirectCall doesnt use UUID
                      }
  result <- callback lambdaOptions
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
