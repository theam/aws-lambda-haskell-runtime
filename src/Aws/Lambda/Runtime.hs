{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Aws.Lambda.Runtime
  ( runLambda
  , Runtime.LambdaResult(..)
  , Runtime.DispatcherStrategy(..)
  , Runtime.DispatcherOptions(..)
  , Runtime.ApiGatewayDispatcherOptions(..)
  , Runtime.defaultDispatcherOptions
  , Error.Parsing(..)
  ) where

import Control.Exception.Safe.Checked
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad (forever)
import System.IO (hFlush, stdout, stderr)
import qualified Network.HTTP.Client as Http

import Data.Aeson
import Data.IORef
import qualified Data.ByteString.Lazy.Char8 as LazyByteString

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.Publish as Publish
import qualified Control.Exception as Unchecked

-- | Runs the user @haskell_lambda@ executable and posts back the
-- results. This is called from the layer's @main@ function.
runLambda :: forall context. IO context -> Runtime.RunCallback context -> IO ()
runLambda initializeCustomContext callback = do
  manager <- Http.newManager httpManagerSettings
  customContext <- initializeCustomContext
  customContextRef <- newIORef customContext
  forever $ do
    lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
    event     <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing
    context   <- Context.initialize @context customContextRef event `catch` errorParsing `catch` variableNotSet

    (((invokeAndRun callback manager lambdaApi event context
      `Checked.catch` \err -> Publish.parsingError err lambdaApi context manager)
      `Checked.catch` \err -> Publish.invocationError err lambdaApi context manager)
      `Checked.catch` \(err :: Error.EnvironmentVariableNotSet) -> Publish.runtimeInitError err lambdaApi context manager)
      `Unchecked.catch` \err -> Publish.invocationError err lambdaApi context manager

httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
  { Http.managerResponseTimeout = Http.responseTimeoutNone
  }

invokeAndRun
  :: Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Runtime.RunCallback context
  -> Http.Manager
  -> String
  -> ApiInfo.Event
  -> Context.Context context
  -> IO ()
invokeAndRun callback manager lambdaApi event context = do
  result <- invokeWithCallback callback event context

  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

invokeWithCallback
  :: Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Runtime.RunCallback c
  -> ApiInfo.Event
  -> Context.Context c
  -> IO Runtime.LambdaResult
invokeWithCallback callback event context = do
  handlerName <- Environment.handlerName
  let lambdaOptions = Runtime.LambdaOptions
                      { eventObject = LazyByteString.unpack $ ApiInfo.event event
                      , functionHandler = handlerName
                      , executionUuid = ""  -- DirectCall doesnt use UUID
                      , contextObject = context
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
errorParsing Error.Parsing{..} =
  error ("Failed parsing " <> errorMessage <> ", got" <> actualValue)

-- | Flush standard output ('stdout') and standard error output ('stderr') handlers
flushOutput :: IO ()
flushOutput = do
  hFlush stdout
  hFlush stderr