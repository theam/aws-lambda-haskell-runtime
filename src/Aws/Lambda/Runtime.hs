module Aws.Lambda.Runtime
  ( runLambda
  ) where

import Control.Exception.Safe.Checked
import qualified Network.HTTP.Client as Http

import qualified Aws.Lambda.Runtime.ApiInfo as ApiInfo
import qualified Aws.Lambda.Runtime.Context as Context
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Aws.Lambda.Runtime.IPC as IPC
import qualified Aws.Lambda.Runtime.Publish as Publish

runLambda
  :: Http.Manager
  -> IO ()
runLambda manager = do
  lambdaApi <- Environment.apiEndpoint `catch` variableNotSet
  event     <- ApiInfo.fetchEvent manager lambdaApi `catch` errorParsing
  context   <- Context.initialize event `catch` errorParsing `catch` variableNotSet
  ((invokeAndRun manager lambdaApi event context
    `catch` \err -> Publish.parsingError err lambdaApi context manager)
    `catch` \err -> Publish.invocationError err lambdaApi context manager)
    `catch` \(err :: Error.EnvironmentVariableNotSet) -> Publish.runtimeInitError err lambdaApi context manager

invokeAndRun
  :: Throws Error.Parsing
  => Throws Error.Invocation
  => Throws Error.EnvironmentVariableNotSet
  => Http.Manager
  -> String
  -> ApiInfo.Event
  -> Context.Context
  -> IO ()
invokeAndRun manager lambdaApi event context = do
  result    <- IPC.invoke (ApiInfo.event event) context
  Publish.result result lambdaApi context manager
    `catch` \err -> Publish.invocationError err lambdaApi context manager

variableNotSet :: Error.EnvironmentVariableNotSet -> IO a
variableNotSet (Error.EnvironmentVariableNotSet env) =
  error ("Error initializing, variable not set: " <> env)

errorParsing :: Error.Parsing -> IO a
errorParsing Error.Parsing{..} =
  error ("Failed parsing " <> errorMessage <> ", got" <> actualValue)
