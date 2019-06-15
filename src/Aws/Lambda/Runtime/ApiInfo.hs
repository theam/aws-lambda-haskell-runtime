module Aws.Lambda.Runtime.ApiInfo
  ( Event(..)
  , fetchEvent
  ) where

import qualified Network.HTTP.Client as Http
import qualified Data.ByteString.Lazy as Lazy

import qualified Aws.Lambda.Runtime.Api.Endpoints as Endpoints

data Event = Event
  { deadlineMs :: !Int
  , traceId :: !String
  , awsRequestId :: !String
  , invokedFunctionArn :: !String
  , event :: !Lazy.ByteString
  }

fetchEvent :: Http.Manager -> String -> IO Event
fetchEvent manager lambdaApi = do
  response <- fetchApiData manager lambdaApi
  body <- Http.responseBody response
  headers <- Http.responseHeaders response
  foldM reduceEvent (Event { event = body }) headers

fetchApiData :: Http.Manager -> String -> IO (Response Lazy.ByteString)
fetchApiData manager lambdaApi = do
  request <- Http.parseRequest (Endpoints.nextInvocation lambdaApi)
  Http.httpLbs request manager

reduceEvent :: Event -> Http.Header -> IO Event
reduceEvent event header =
  case header of
    ("Lambda-Runtime-Deadline-Ms", value) ->
      case Read.readMaybe value of
        Just ms -> pure event { deadlineMs = ms }
        Nothing -> throw (Error.Parsing "deadlineMs" value)

    ("Lambda-Runtime-Trace-Id", value) ->
      event { traceId = decodeUtf8 value }

    ("Lambda-Runtime-Aws-Request-Id", value) ->
      event { awsRequest = decodeUtf8 value }

    ("Lambda-Runtime-Invoked-Function-Arn", value) ->
      event { invokedFunctionArn = decodeUtf8 value }

    _ ->
      event

httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
  { Http.managerResponseTimeout = Http.responseTimeoutNone
  }
