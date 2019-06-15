module Aws.Lambda.Runtime.Publish
  ( result
  , invocationError
  , parsingError
  , runtimeInitError
  ) where

import Data.Aeson
import Control.Monad (void)
import qualified Network.HTTP.Client as Http

import Aws.Lambda.Runtime.Context (Context(..))
import qualified Aws.Lambda.Runtime.API.Endpoints as Endpoints
import qualified Aws.Lambda.Runtime.Error as Error
import Aws.Lambda.Runtime.Result (LambdaResult(..))

result :: LambdaResult -> String -> Context -> Http.Manager -> IO ()
result res lambdaApi context =
  publish res (Endpoints.response lambdaApi $ awsRequestId context)
    context

invocationError :: Error.Invocation -> String -> Context -> Http.Manager -> IO ()
invocationError err lambdaApi context =
  publish err (Endpoints.invocationError lambdaApi $ awsRequestId context)
    context

parsingError :: Error.Parsing -> String -> Context -> Http.Manager -> IO ()
parsingError err lambdaApi context =
  publish err (Endpoints.invocationError lambdaApi $ awsRequestId context)
    context

runtimeInitError :: ToJSON err => err -> String -> Context -> Http.Manager -> IO ()
runtimeInitError err lambdaApi =
  publish err (Endpoints.runtimeInitError lambdaApi)

publish :: ToJSON err => err -> Endpoints.Endpoint -> Context -> Http.Manager -> IO ()
publish err (Endpoints.Endpoint endpoint) Context{..} manager = do
  rawRequest <- Http.parseRequest endpoint
  let request = rawRequest
                { Http.method = "POST"
                , Http.requestBody = Http.RequestBodyLBS (encode err)
                }
  void $ Http.httpNoBody request manager