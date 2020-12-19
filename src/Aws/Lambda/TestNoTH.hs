{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Aws.Lambda.TestNoTH (Handler (..), IOHandler, HandlerName, Handlers, runNoTH, mainNoTH) where

import Aws.Lambda
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStr)

type IOHandler = Handler IO

type HandlerName = Text

type Handlers m context request response error =
  HM.HashMap HandlerName (Handler m context request response error)

data Handler m context request response error where
  StandaloneLambdaHandler :: (request -> Context context -> m (Either error response)) -> Handler m context request response error
  APIGatewayHandler :: (ApiGatewayRequest request -> Context context -> m (Either (ApiGatewayResponse error) (ApiGatewayResponse response))) -> Handler m context request response error

mainNoTH ::
  forall m context request response error.
  MonadIO m =>
  MonadCatch m =>
  ToLambdaResponseBody error =>
  ToLambdaResponseBody response =>
  ToApiGatewayResponseBody error =>
  ToApiGatewayResponseBody response =>
  FromJSON (ApiGatewayRequest request) =>
  FromJSON request =>
  Typeable request =>
  DispatcherOptions ->
  (forall a. m a -> IO a) ->
  Handlers m context request response error ->
  IO context ->
  IO ()
mainNoTH options mToIO handlers initializeContext =
  runLambda initializeContext (runNoTH options mToIO handlers)

-- TODO: GADT for Handler to reduce constraints?
runNoTH ::
  forall m context request response error.
  MonadIO m =>
  MonadCatch m =>
  ToLambdaResponseBody error =>
  ToLambdaResponseBody response =>
  ToApiGatewayResponseBody error =>
  ToApiGatewayResponseBody response =>
  FromJSON (ApiGatewayRequest request) =>
  FromJSON request =>
  Typeable request =>
  DispatcherOptions ->
  (forall a. m a -> IO a) ->
  Handlers m context request response error ->
  LambdaOptions context ->
  IO (Either LambdaError LambdaResult)
runNoTH dispatcherOptions mToIO handlers (LambdaOptions eventObject functionHandler _executionUuid contextObject) = do
  let asIOCallbacks = HM.map (mToIO . handlerToCallback dispatcherOptions eventObject contextObject) handlers
  case HM.lookup functionHandler asIOCallbacks of
    Just handlerToCall -> handlerToCall
    Nothing -> error "no handler matches"

-- TODO: Design for a bit more type safety
handlerToCallback ::
  forall m context request response error.
  MonadIO m =>
  MonadCatch m =>
  ToLambdaResponseBody error =>
  ToLambdaResponseBody response =>
  ToApiGatewayResponseBody error =>
  ToApiGatewayResponseBody response =>
  FromJSON request =>
  Typeable request =>
  FromJSON (ApiGatewayRequest request) =>
  DispatcherOptions ->
  RawEventObject ->
  Context context ->
  Handler m context request response error ->
  m (Either LambdaError LambdaResult)
handlerToCallback dispatcherOptions rawEventObject context handlerToCall =
  call `catch` handleError
  where
    call =
      -- TODO: Reorganize
      case handlerToCall of
        StandaloneLambdaHandler handler ->
          case decodeObj @request rawEventObject of
            Right request ->
              either
                (Left . StandaloneLambdaError . toStandaloneLambdaResponse)
                (Right . StandaloneLambdaResult . toStandaloneLambdaResponse)
                <$> handler request context
            Left err -> return . Left . StandaloneLambdaError . toStandaloneLambdaResponse $ err
        APIGatewayHandler handler -> do
          case decodeObj @(ApiGatewayRequest request) rawEventObject of
            Right request ->
              either
                (Left . ApiGatewayLambdaError . fmap toApiGatewayResponseBody)
                (Right . ApiGatewayResult . fmap toApiGatewayResponseBody)
                <$> handler request context
            Left err -> apiGatewayErr 400 . toApiGatewayResponseBody . Text.pack . show $ err

    -- TODO: Design for a bit more type safety
    handleError (exception :: SomeException) = do
      liftIO $ hPutStr stderr . show $ exception
      case handlerToCall of
        StandaloneLambdaHandler _ ->
          return . Left . StandaloneLambdaError . toStandaloneLambdaResponse . Text.pack . show $ exception
        APIGatewayHandler _ ->
           if propagateImpureExceptions . apiGatewayDispatcherOptions $ dispatcherOptions
              then apiGatewayErr 500 . toApiGatewayResponseBody . Text.pack . show $ exception
              else apiGatewayErr 500 . toApiGatewayResponseBody . Text.pack $ "Something went wrong."

    apiGatewayErr statusCode =
      pure . Left . ApiGatewayLambdaError . mkApiGatewayResponse statusCode
