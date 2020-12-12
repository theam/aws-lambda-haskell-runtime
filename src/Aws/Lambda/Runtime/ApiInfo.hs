module Aws.Lambda.Runtime.ApiInfo
  ( Event (..),
    fetchEvent,
  )
where

import qualified Aws.Lambda.Runtime.API.Endpoints as Endpoints
import qualified Aws.Lambda.Runtime.Error as Error
import Control.Exception (IOException)
import Control.Exception.Safe.Checked
import qualified Control.Monad as Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Text.Read as Read

-- | Event that is fetched out of the AWS Lambda API
data Event = Event
  { deadlineMs :: !Int,
    traceId :: !Text,
    awsRequestId :: !Text,
    invokedFunctionArn :: !Text,
    event :: !Lazy.ByteString
  }
  deriving (Show)

-- | Performs a GET to the endpoint that provides the next event
fetchEvent :: Throws Error.Parsing => Http.Manager -> Text -> IO Event
fetchEvent manager lambdaApi = do
  response <- fetchApiData manager lambdaApi
  let body = Http.responseBody response
      headers = Http.responseHeaders response
  Monad.foldM reduceEvent (initialEvent body) headers

fetchApiData :: Http.Manager -> Text -> IO (Http.Response Lazy.ByteString)
fetchApiData manager lambdaApi = do
  let Endpoints.Endpoint endpoint = Endpoints.nextInvocation lambdaApi
  request <- Http.parseRequest . Text.unpack $ endpoint
  keepRetrying $ Http.httpLbs request manager

reduceEvent :: Throws Error.Parsing => Event -> (Http.HeaderName, ByteString) -> IO Event
reduceEvent event header =
  case header of
    ("Lambda-Runtime-Deadline-Ms", value) ->
      case Read.readMaybe $ ByteString.unpack value of
        Just ms -> pure event {deadlineMs = ms}
        Nothing -> throw (Error.Parsing "Could not parse deadlineMs." (pack $ ByteString.unpack value) "deadlineMs")
    ("Lambda-Runtime-Trace-Id", value) ->
      pure event {traceId = pack $ ByteString.unpack value}
    ("Lambda-Runtime-Aws-Request-Id", value) ->
      pure event {awsRequestId = pack $ ByteString.unpack value}
    ("Lambda-Runtime-Invoked-Function-Arn", value) ->
      pure event {invokedFunctionArn = pack $ ByteString.unpack value}
    _ ->
      pure event

initialEvent :: Lazy.ByteString -> Event
initialEvent body =
  Event
    { deadlineMs = 0,
      traceId = "",
      awsRequestId = "",
      invokedFunctionArn = "",
      event = body
    }

keepRetrying :: IO (Http.Response Lazy.ByteString) -> IO (Http.Response Lazy.ByteString)
keepRetrying action = do
  result <- try action :: IO (Either IOException (Http.Response Lazy.ByteString))
  case result of
    Right success -> pure success
    _ -> keepRetrying action
