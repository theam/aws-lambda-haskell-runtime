{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Publishing of results/errors back to the
-- AWS Lambda runtime API
module Aws.Lambda.Runtime.Publish
  ( result,
    invocationError,
    parsingError,
    handlerNotFoundError,
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
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as Http
import Aws.Lambda.Runtime.Configuration
import Aws.Lambda.Runtime.Error
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)

-- | Publishes the result back to AWS Lambda
result :: LambdaResult handlerType -> Text -> Context context -> Http.Manager -> IO ()
result lambdaResult lambdaApi context manager = do
  let Endpoints.Endpoint endpoint = Endpoints.response lambdaApi (awsRequestId context)
  rawRequest <- Http.parseRequest . unpack $ endpoint

  let requestBody = case lambdaResult of
        (StandaloneLambdaResult (StandaloneLambdaResponseBodyPlain res)) ->
          Http.RequestBodyBS (T.encodeUtf8 res)
        (StandaloneLambdaResult (StandaloneLambdaResponseBodyJson res)) ->
          Http.RequestBodyLBS res
        (APIGatewayResult res) -> Http.RequestBodyLBS (encode res)
        (ALBResult res) -> Http.RequestBodyLBS (encode res)
      request =
        rawRequest
          { Http.method = "POST",
            Http.requestBody = requestBody
          }

  void $ Http.httpNoBody request manager

-- | Publishes an invocation error back to AWS Lambda
invocationError :: ErrorLogger -> Error.Invocation -> Text -> Context context -> Http.Manager -> IO ()
invocationError logger (Error.Invocation err) lambdaApi context =
  publish logger InvocationError err (Endpoints.invocationError lambdaApi $ awsRequestId context) context

-- | Publishes a parsing error back to AWS Lambda
parsingError :: ErrorLogger -> Error.Parsing -> Text -> Context context -> Http.Manager -> IO ()
parsingError logger err lambdaApi context =
  publish
    logger
    InvocationError
    (encode err)
    (Endpoints.invocationError lambdaApi $ awsRequestId context)
    context

-- | Publishes a HandlerNotFound error back to AWS Lambda
handlerNotFoundError :: ErrorLogger -> Error.HandlerNotFound -> Text -> Context context -> Http.Manager -> IO ()
handlerNotFoundError logger err lambdaApi context =
  publish
    logger
    InvocationError
    (encode err)
    (Endpoints.invocationError lambdaApi $ awsRequestId context)
    context

-- | Publishes a runtime initialization error back to AWS Lambda
runtimeInitError :: ToJSON err => ErrorLogger -> err -> Text -> Context context -> Http.Manager -> IO ()
runtimeInitError logger err lambdaApi =
  publish logger Error.InitializationError (encode err) (Endpoints.runtimeInitError lambdaApi)

publish :: ErrorLogger -> Error.ErrorType -> LBS.ByteString -> Endpoints.Endpoint -> Context context -> Http.Manager -> IO ()
publish logger errorType err (Endpoints.Endpoint endpoint) context manager = do
  logger context errorType $ decodeUtf8 $ toStrict err
  rawRequest <- Http.parseRequest . unpack $ endpoint

  let requestBody = Http.RequestBodyLBS err
      request =
        rawRequest
          { Http.method = "POST",
            Http.requestBody = requestBody
          }

  void $ Http.httpNoBody request manager
