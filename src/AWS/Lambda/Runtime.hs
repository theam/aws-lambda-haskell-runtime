module AWS.Lambda.Runtime where

import           Relude

import           Data.Aeson

import qualified AWS.Lambda.Context            as Context
import qualified AWS.Lambda.Env                as Env
import qualified AWS.Lambda.Error              as Error

defaultMaxRetries :: Int
defaultMaxRetries = 3

type Handler e o = e -> Context.Context -> IO (Either Error.HandlerError o)

start :: (FromJSON e, ToJSON o) => Handler e o -> Option runtime -> IO ()
start f = startWithConfig f Env.envConfig

startWithConfig
  :: (FromJSON e, ToJSON o)
  => Handler e o
  -> Env.ProviderCapability
  -> Option runtime
  -> IO ()
startWithConfig f Env.ProviderCapability {..} r = do
  let showError = error . toText . Error.runtimeErrorMsg
  -- endpoint         <- either showError id <$> getRuntimeApiEndpoint
  functionSettings <- either showError id <$> getFunctionSettings
  runtimeClient    <- either showError id <$> newRuntimeClient
  startWithRuntimeClient f functionSettings runtimeClient


startWithRuntimeClient
  :: (FromJSON e, ToJSON o)
  => Handler e o
  -> Env.FunctionSettings
  -> RuntimeClient
  -> IO ()
startWithRuntimeClient = undefined

data Runtime e o = Runtime
  { runtimeClient :: RuntimeClient
  , handler :: Handler e o
  , maxRetries :: Int
  , settings :: Env.FunctionSettings
  }

data RuntimeClient

newRuntimeClient :: IO (Either Error.RuntimeError RuntimeClient)
newRuntimeClient = undefined

eventResponse :: RuntimeClient -> String -> w -> Maybe a
eventResponse = undefined

eventError :: RuntimeClient -> String -> Maybe a
eventError = undefined

startRuntime :: Runtime e o -> IO ()
startRuntime = loopRuntime
 where
  loopRuntime Runtime {..} = do
    (event, ctx) <- getNextEvent 0 Nothing
    let requestId                 = Context.awsRequestId ctx
    let err :: Error.RuntimeError = undefined
    res <- event ctx
    case res of
      Just (response :: Int) -> do
        let responseBytes = encode response
        case eventResponse runtimeClient requestId responseBytes of
          Just _ ->
            putTextLn
              $  "Response for"
              <> show requestId
              <> "accepted by Runtime API"
          Nothing -> do
            putTextLn
              $  "Could not send response for "
              <> show requestId
              <> " to Runtime API: "
              <> show err
            unless
              (Error.runtimeErrorRecoverable err)
              (putTextLn
                "Error is not recoverable, sending fail_init signal and panicking."
              )

      Nothing -> do
        putTextLn
          $  "Handler returned an error for "
          <> show requestId
          <> ": "
          <> show err
          <> ""

        case eventError runtimeClient requestId of
          Just _  -> putTextLn "Error accepted by Runtime API"
          Nothing -> do
            putTextLn
              $  "Could not send response for "
              <> show requestId
              <> " to Runtime API: "
              <> show err

            unless
              (Error.runtimeErrorRecoverable err)
              (putTextLn
                "Error is not recoverable, sending fail_init signal and panicking."
              )

getNextEvent :: Int -> Maybe error -> IO (e, Context.Context)
getNextEvent = undefined
