{-# LANGUAGE GADTs #-}

-- | Publishing of results/errors back to the
-- AWS Lambda runtime API
module Aws.Lambda.Runtime.Publish
  ( result,
    invocationError,
    parsingError,
    runtimeInitError,
  )
where

import qualified Aws.Lambda.Runtime.API.Endpoints as Endpoints
import Aws.Lambda.Runtime.Common
import Aws.Lambda.Runtime.Context (Context (..))
import qualified Aws.Lambda.Runtime.Error as Error
import Aws.Lambda.Runtime.StandaloneLambda.Types
import Control.Monad (void)
import Data.Aeson
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as Http

-- | Publishes the result back to AWS Lambda
result :: LambdaResult t -> Text -> Context context -> Http.Manager -> IO ()
result lambdaResult lambdaApi context manager = do
  let Endpoints.Endpoint endpoint = Endpoints.response lambdaApi (awsRequestId context)
  rawRequest <- Http.parseRequest . unpack $ endpoint

  let requestBody = case lambdaResult of
        (StandaloneLambdaResult res) -> Http.RequestBodyBS (T.encodeUtf8 . unStandaloneLambdaResponseBody $ res)
        (APIGatewayResult res) -> Http.RequestBodyLBS (encode res)
        (ALBResult res) -> Http.RequestBodyLBS (encode res)
      request =
        rawRequest
          { Http.method = "POST",
            Http.requestBody = requestBody
          }

  void $ Http.httpNoBody request manager

-- | Publishes an invocation error back to AWS Lambda
invocationError :: Error.Invocation -> Text -> Context context -> Http.Manager -> IO ()
invocationError err lambdaApi context =
  publish err (Endpoints.invocationError lambdaApi $ awsRequestId context) context

-- | Publishes a parsing error back to AWS Lambda
parsingError :: Error.Parsing -> Text -> Context context -> Http.Manager -> IO ()
parsingError err lambdaApi context =
  publish
    err
    (Endpoints.invocationError lambdaApi $ awsRequestId context)
    context

-- | Publishes a runtime initialization error back to AWS Lambda
runtimeInitError :: ToJSON err => err -> Text -> Context context -> Http.Manager -> IO ()
runtimeInitError err lambdaApi =
  publish err (Endpoints.runtimeInitError lambdaApi)

publish :: ToJSON err => err -> Endpoints.Endpoint -> Context context -> Http.Manager -> IO ()
publish err (Endpoints.Endpoint endpoint) _context manager = do
  rawRequest <- Http.parseRequest . unpack $ endpoint

  let requestBody = Http.RequestBodyLBS (encode err)
      request =
        rawRequest
          { Http.method = "POST",
            Http.requestBody = requestBody
          }

  void $ Http.httpNoBody request manager
