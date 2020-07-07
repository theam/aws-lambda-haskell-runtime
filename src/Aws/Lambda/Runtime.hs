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
import Control.Exception
import Control.Monad (forever)
import Data.Aeson
import Data.IORef
import qualified Network.HTTP.Client as Http
import System.IO (hFlush, stderr, stdout)

runLambda ::
  ( FromJSON event,
    Runtime.ToLambdaResponseBody error,
    Runtime.ToLambdaResponseBody result
  ) =>
  IO context ->
  (event -> Context.Context context -> IO (Either error result)) ->
  IO ()
runLambda initializer userCode =
  runtime initializer (wrapStandaloneLambda userCode)

wrapStandaloneLambda ::
  ( FromJSON event,
    Runtime.ToLambdaResponseBody error,
    Runtime.ToLambdaResponseBody result
  ) =>
  (event -> Context.Context context -> IO (Either error result)) ->
  Runtime.LambdaOptions context ->
  IO (Either Runtime.LambdaError Runtime.LambdaResult)
wrapStandaloneLambda userCode Runtime.LambdaOptions {..} =
  case eitherDecode eventObject of
    Left err ->
      pure . Left . Runtime.StandaloneLambdaError $ Runtime.toStandaloneLambdaResponse err
    Right event -> do
      result <- userCode event contextObject
      case result of
        Left err ->
          pure . Left . Runtime.StandaloneLambdaError $ Runtime.toStandaloneLambdaResponse err
        Right ok ->
          pure . Right . Runtime.StandaloneLambdaResult $ Runtime.toStandaloneLambdaResponse ok

runtime :: forall context. IO context -> Runtime.RunCallback context -> IO ()
runtime initializeCustomContext callback = do
  manager <- Http.newManager httpManagerSettings
  customContext <- initializeCustomContext
  customContextRef <- newIORef customContext
  context <- Context.initialize @context customContextRef `catch` errorParsing `catch` variableNotSet
  forever $ do
    lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
    event <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing
    -- Purposefully shadowing to prevent using the initial "empty" context
    context <- Context.setEventData context event
    invokeAndRun callback manager lambdaApi event context
      `catches` [ Handler $ \err -> Publish.parsingError err lambdaApi context manager,
                  Handler $ \err -> Publish.invocationError err lambdaApi context manager,
                  Handler $ \(err :: Error.EnvironmentVariableNotSet) -> Publish.runtimeInitError err lambdaApi context manager,
                  Handler $ \err -> Publish.invocationError err lambdaApi context manager
                ]

httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
    { Http.managerResponseTimeout = Http.responseTimeoutNone
    }

invokeAndRun ::
  Runtime.RunCallback context ->
  Http.Manager ->
  String ->
  ApiInfo.Event ->
  Context.Context context ->
  IO ()
invokeAndRun callback manager lambdaApi event context = do
  result <- invokeWithCallback callback event context
  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

invokeWithCallback ::
  Runtime.RunCallback context ->
  ApiInfo.Event ->
  Context.Context context ->
  IO Runtime.LambdaResult
invokeWithCallback callback event context = do
  handlerName <- Environment.handlerName
  let lambdaOptions =
        Runtime.LambdaOptions
          { eventObject = ApiInfo.event event,
            functionHandler = handlerName,
            executionUuid = "", -- DirectCall doesnt use UUID
            contextObject = context
          }
  result <- callback lambdaOptions
  -- Flush output to insure output goes into CloudWatch logs
  flushOutput
  case result of
    Left lambdaError -> case lambdaError of
      Runtime.StandaloneLambdaError err ->
        throw $ Error.Invocation $ toJSON err
      Runtime.ApiGatewayLambdaError err ->
        throw $ Error.Invocation $ toJSON err
    Right value ->
      pure value

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
