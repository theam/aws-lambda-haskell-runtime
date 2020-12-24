{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Aws.Lambda.Runtime
  ( runLambda,
    Runtime.LambdaResult (..),
    Runtime.ApiGatewayDispatcherOptions (..),
    Error.Parsing (..),
  )
where

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.Publish as Publish
import qualified Control.Exception as Unchecked
import Control.Exception.Safe.Checked (Throws, catch, throw)
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad (forever)
import Data.Aeson (ToJSON (toJSON))
import Data.IORef (newIORef)
import Data.Text (Text, unpack)
import qualified Network.HTTP.Client as Http
import System.IO (hFlush, stderr, stdout)

-- | Runs the user @haskell_lambda@ executable and posts back the
-- results. This is called from the layer's @main@ function.
runLambda :: forall context t. IO context -> Runtime.RunCallback t context -> IO ()
runLambda initializeCustomContext callback = do
  manager <- Http.newManager httpManagerSettings
  customContext <- initializeCustomContext
  customContextRef <- newIORef customContext
  context <- Context.initialize @context customContextRef `catch` errorParsing `catch` variableNotSet
  forever $ do
    lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
    event <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing

    -- Purposefully shadowing to prevent using the initial "empty" context
    context <- Context.setEventData context event

    ( ( ( invokeAndRun callback manager lambdaApi event context
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
  Throws Error.EnvironmentVariableNotSet =>
  Runtime.RunCallback t context ->
  Http.Manager ->
  Text ->
  ApiInfo.Event ->
  Context.Context context ->
  IO ()
invokeAndRun callback manager lambdaApi event context = do
  result <- invokeWithCallback callback event context

  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

invokeWithCallback ::
  Throws Error.Invocation =>
  Throws Error.EnvironmentVariableNotSet =>
  Runtime.RunCallback t context ->
  ApiInfo.Event ->
  Context.Context context ->
  IO (Runtime.LambdaResult t)
invokeWithCallback callback event context = do
  handlerName <- Runtime.HandlerName <$> Environment.handlerName
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
      Runtime.APIGatewayLambdaError err ->
        throw $ Error.Invocation $ toJSON err
      Runtime.ALBLambdaError err ->
        throw $ Error.Invocation $ toJSON err
    Right value ->
      pure value

variableNotSet :: Error.EnvironmentVariableNotSet -> IO a
variableNotSet (Error.EnvironmentVariableNotSet env) =
  error ("Error initializing, variable not set: " <> unpack env)

errorParsing :: Error.Parsing -> IO a
errorParsing Error.Parsing {..} =
  error ("Failed parsing " <> unpack errorMessage <> ", got" <> unpack actualValue)

-- | Flush standard output ('stdout') and standard error output ('stderr') handlers
flushOutput :: IO ()
flushOutput = do
  hFlush stdout
  hFlush stderr