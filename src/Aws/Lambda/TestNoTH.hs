{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Aws.Lambda.TestNoTH
  ( Handler (..),
    HandlerName,
    Handlers,
    runNoTH,
    mainNoTH,
    addStandaloneLambdaHandler,
    addAPIGatewayHandler,
    runLambdaHaskellRuntime,
  )
where

import Aws.Lambda
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State as State
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStr)
import Aws.Lambda.Runtime.Common

type HandlerName = Text

type Handlers t m context request response error =
  HM.HashMap HandlerName (Handler t m context request response error)

-- TODO: MonadError instead of Either?
type StandaloneCallback m context request response error =
  (request -> Context context -> m (Either error response))

type APIGatewayCallback m context request response error =
  (ApiGatewayRequest request -> Context context -> m (Either (ApiGatewayResponse error) (ApiGatewayResponse response)))

data Handler (t :: HandlerType) m context request response error where
  StandaloneLambdaHandler :: StandaloneCallback m context request response error -> Handler 'StandaloneHandlerType m context request response error
  APIGatewayHandler :: APIGatewayCallback m context request response error -> Handler 'APIGatewayHandlerType m context request response error

newtype HandlersM (t :: HandlerType) m context request response error a = HandlersM
  {runHandlersM :: StateT (Handlers t m context request response error) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (Handlers t m context request response error)
    )

addStandaloneLambdaHandler ::
  HandlerName ->
  StandaloneCallback m context request response error ->
  HandlersM 'StandaloneHandlerType m context request response error ()
addStandaloneLambdaHandler handlerName handler =
  State.modify (HM.insert handlerName (StandaloneLambdaHandler handler))

addAPIGatewayHandler ::
  HandlerName ->
  APIGatewayCallback m context request response error ->
  HandlersM 'APIGatewayHandlerType m context request response error ()
addAPIGatewayHandler handlerName handler =
  State.modify (HM.insert handlerName (APIGatewayHandler handler))

runLambdaHaskellRuntime ::
  forall t m context request response error.
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
  IO context ->
  (forall a. m a -> IO a) ->
  HandlersM t m context request response error () ->
  IO ()
runLambdaHaskellRuntime options initializeContext mToIO initHandlers = do
  handlers <- fmap snd . flip runStateT HM.empty . runHandlersM $ initHandlers
  runLambda initializeContext (runNoTH options mToIO handlers)

mainNoTH ::
  forall t m context request response error.
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
  Handlers t m context request response error ->
  IO context ->
  IO ()
mainNoTH options mToIO handlers initializeContext =
  runLambda initializeContext (runNoTH options mToIO handlers)

-- TODO: GADT for Handler to reduce constraints?
runNoTH ::
  forall t m context request response error.
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
  Handlers t m context request response error ->
  LambdaOptions context ->
  IO (Either (LambdaError t) LambdaResult)
runNoTH dispatcherOptions mToIO handlers (LambdaOptions eventObject functionHandler _executionUuid contextObject) = do
  let asIOCallbacks = HM.map (mToIO . handlerToCallback dispatcherOptions eventObject contextObject) handlers
  case HM.lookup functionHandler asIOCallbacks of
    Just handlerToCall -> handlerToCall
    Nothing -> error "no handler matches"

-- TODO: Design for a bit more type safety
handlerToCallback ::
  forall t m context request response error.
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
  Handler t m context request response error ->
  m (Either (LambdaError t) LambdaResult)
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
