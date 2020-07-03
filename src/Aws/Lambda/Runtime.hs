{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Aws.Lambda.Runtime
  ( runLambda,
    Runtime.LambdaResult (..),
    Runtime.DispatcherStrategy (..),
    Runtime.DispatcherOptions (..),
    Runtime.ApiGatewayDispatcherOptions (..),
    Runtime.defaultDispatcherOptions,
    Error.Parsing (..),
  )
where

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.Publish as Publish
import qualified Aws.Lambda.Utilities as Utilities
import qualified Control.Exception as Unchecked
import Control.Exception.Safe.Checked
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad (forever)
import Data.Aeson
import Data.IORef
import qualified Network.HTTP.Client as Http
import System.IO (hFlush, stderr, stdout)

-- | Runs the user @haskell_lambda@ executable and posts back the
-- results. This is called from the layer's @main@ function.
runLambda ::
  forall context event error result.
  FromJSON event =>
  ToJSON error =>
  ToJSON result =>
  IO context ->
  (Context.Context context -> event -> IO (Either error result)) ->
  IO ()
runLambda initializeCustomContext userCode = do
  manager <- Http.newManager httpManagerSettings
  customContext <- initializeCustomContext
  customContextRef <- newIORef customContext
  context <- Context.initialize @context customContextRef `catch` errorParsing `catch` variableNotSet
  forever $ do
    lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
    event <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing
    -- Purposefully shadowing to prevent using the initial "empty" context
    context <- Context.setEventData context event
    ( ( ( invokeAndRun userCode manager lambdaApi event context
            `Checked.catch` \err -> Publish.parsingError err lambdaApi context manager
        )
          `Checked.catch` \err -> Publish.invocationError err lambdaApi context manager
      )
        `Checked.catch` \(err :: Error.EnvironmentVariableNotSet) -> Publish.runtimeInitError err lambdaApi context manager
      )
      `Unchecked.catch` \err -> Publish.invocationError err lambdaApi context manager

httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
    { Http.managerResponseTimeout = Http.responseTimeoutNone
    }

invokeAndRun ::
  Throws Error.Invocation =>
  FromJSON event =>
  ToJSON error =>
  ToJSON result =>
  (Context.Context context -> event -> IO (Either error result)) ->
  Http.Manager ->
  String ->
  ApiInfo.Event ->
  Context.Context context ->
  IO ()
invokeAndRun userCode manager lambdaApi event context = do
  result <- invokeWithCallback userCode event context
  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

invokeWithCallback ::
  Throws Error.Invocation =>
  FromJSON event =>
  ToJSON error =>
  ToJSON result =>
  (Context.Context context -> event -> IO (Either error result)) ->
  ApiInfo.Event ->
  Context.Context context ->
  IO Runtime.LambdaResult
invokeWithCallback userCode event context = do
  eventObject <-
    case eitherDecode $ ApiInfo.event event of
      Right obj -> pure obj
      Left err -> throw $ Error.Invocation $ toJSON err
  result <- userCode context eventObject
  -- Flush output to insure output goes into CloudWatch logs
  flushOutput
  case result of
    Left err ->
      throw $ Error.Invocation $ toJSON err
    -- Left lambdaError -> case lambdaError of
    --   Runtime.StandaloneLambdaError err ->
    --     throw $ Error.Invocation $ toJSON err
    --   Runtime.ApiGatewayLambdaError err ->
    --     throw $ Error.Invocation $ toJSON err
    Right value ->
      pure (Runtime.StandaloneLambdaResult (Runtime.LambdaResponseBody $ Utilities.toJSONText value))

variableNotSet :: Error.EnvironmentVariableNotSet -> IO a
variableNotSet (Error.EnvironmentVariableNotSet env) =
  error ("Error initializing, variable not set: " <> env)

errorParsing :: Error.Parsing -> IO a
errorParsing Error.Parsing {..} =
  error ("Failed parsing " <> errorMessage <> ", got" <> actualValue)

-- | Flush standard output ('stdout') and standard error output ('stderr') handlers
flushOutput :: IO ()
flushOutput = do
  hFlush stdout
  hFlush stderr
