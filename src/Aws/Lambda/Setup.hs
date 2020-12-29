{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Aws.Lambda.Setup
  ( Handler (..),
    HandlerName (..),
    Handlers,
    run,
    addStandaloneLambdaHandler,
    addAPIGatewayHandler,
    addALBHandler,
    runLambdaHaskellRuntime,
  )
where

import Aws.Lambda.Runtime (runLambda)
import Aws.Lambda.Runtime.ALB.Types
  ( ALBRequest,
    ALBResponse,
    ToALBResponseBody (..),
    mkALBResponse,
  )
import Aws.Lambda.Runtime.APIGateway.Types
  ( ApiGatewayDispatcherOptions (propagateImpureExceptions),
    ApiGatewayRequest,
    ApiGatewayResponse,
    ToApiGatewayResponseBody (..),
    mkApiGatewayResponse,
  )
import Aws.Lambda.Runtime.Common
  ( HandlerName (..),
    HandlerType (..),
    LambdaError (..),
    LambdaOptions (LambdaOptions),
    LambdaResult (..),
    RawEventObject,
  )
import Aws.Lambda.Runtime.Configuration
  ( DispatcherOptions (apiGatewayDispatcherOptions),
  )
import Aws.Lambda.Runtime.Context (Context)
import Aws.Lambda.Runtime.StandaloneLambda.Types
  ( ToStandaloneLambdaResponseBody (..),
  )
import Aws.Lambda.Utilities (decodeObj)
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch (catch), throwM)
import Control.Monad.State as State
  ( MonadIO (..),
    MonadState,
    StateT (..),
    modify,
  )
import Data.Aeson (FromJSON)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStr)

type Handlers t m context request response error =
  HM.HashMap HandlerName (Handler t m context request response error)

type StandaloneCallback m context request response error =
  (request -> Context context -> m (Either error response))

type APIGatewayCallback m context request response error =
  (ApiGatewayRequest request -> Context context -> m (Either (ApiGatewayResponse error) (ApiGatewayResponse response)))

type ALBCallback m context request response error =
  (ALBRequest request -> Context context -> m (Either (ALBResponse error) (ALBResponse response)))

data Handler (t :: HandlerType) m context request response error where
  StandaloneLambdaHandler :: StandaloneCallback m context request response error -> Handler 'StandaloneHandlerType m context request response error
  APIGatewayHandler :: APIGatewayCallback m context request response error -> Handler 'APIGatewayHandlerType m context request response error
  ALBHandler :: ALBCallback m context request response error -> Handler 'ALBHandlerType m context request response error

newtype HandlersM (t :: HandlerType) m context request response error a = HandlersM
  {runHandlersM :: StateT (Handlers t m context request response error) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (Handlers t m context request response error)
    )

type RuntimeContext (t :: HandlerType) m context request response error =
  ( MonadIO m,
    MonadCatch m,
    ToStandaloneLambdaResponseBody error,
    ToStandaloneLambdaResponseBody response,
    ToApiGatewayResponseBody error,
    ToApiGatewayResponseBody response,
    ToALBResponseBody error,
    ToALBResponseBody response,
    FromJSON (ApiGatewayRequest request),
    FromJSON (ALBRequest request),
    FromJSON request,
    Typeable request
  )

runLambdaHaskellRuntime ::
  RuntimeContext t m context request response error =>
  DispatcherOptions ->
  IO context ->
  (forall a. m a -> IO a) ->
  HandlersM t m context request response error () ->
  IO ()
runLambdaHaskellRuntime options initializeContext mToIO initHandlers = do
  handlers <- fmap snd . flip runStateT HM.empty . runHandlersM $ initHandlers
  runLambda initializeContext (run options mToIO handlers)

run ::
  RuntimeContext t m context request response error =>
  DispatcherOptions ->
  (forall a. m a -> IO a) ->
  Handlers t m context request response error ->
  LambdaOptions context ->
  IO (Either (LambdaError t) (LambdaResult t))
run dispatcherOptions mToIO handlers (LambdaOptions eventObject functionHandler _executionUuid contextObject) = do
  let asIOCallbacks = HM.map (mToIO . handlerToCallback dispatcherOptions eventObject contextObject) handlers
  case HM.lookup functionHandler asIOCallbacks of
    Just handlerToCall -> handlerToCall
    Nothing ->
      throwM $
        userError $
          "Could not find handler '" <> (Text.unpack . unHandlerName $ functionHandler) <> "'."

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

addALBHandler ::
  HandlerName ->
  ALBCallback m context request response error ->
  HandlersM 'ALBHandlerType m context request response error ()
addALBHandler handlerName handler =
  State.modify (HM.insert handlerName (ALBHandler handler))

handlerToCallback ::
  forall t m context request response error.
  RuntimeContext t m context request response error =>
  DispatcherOptions ->
  RawEventObject ->
  Context context ->
  Handler t m context request response error ->
  m (Either (LambdaError t) (LambdaResult t))
handlerToCallback dispatcherOptions rawEventObject context handlerToCall =
  call `catch` handleError
  where
    call =
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
                (Left . APIGatewayLambdaError . fmap toApiGatewayResponseBody)
                (Right . APIGatewayResult . fmap toApiGatewayResponseBody)
                <$> handler request context
            Left err -> apiGatewayErr 400 . toApiGatewayResponseBody . Text.pack . show $ err
        ALBHandler handler ->
          case decodeObj @(ALBRequest request) rawEventObject of
            Right request ->
              either
                (Left . ALBLambdaError . fmap toALBResponseBody)
                (Right . ALBResult . fmap toALBResponseBody)
                <$> handler request context
            Left err -> albErr 400 . toALBResponseBody . Text.pack . show $ err

    handleError (exception :: SomeException) = do
      liftIO $ hPutStr stderr . show $ exception
      case handlerToCall of
        StandaloneLambdaHandler _ ->
          return . Left . StandaloneLambdaError . toStandaloneLambdaResponse . Text.pack . show $ exception
        ALBHandler _ ->
          albErr 500 . toALBResponseBody . Text.pack . show $ exception
        APIGatewayHandler _ ->
          if propagateImpureExceptions . apiGatewayDispatcherOptions $ dispatcherOptions
            then apiGatewayErr 500 . toApiGatewayResponseBody . Text.pack . show $ exception
            else apiGatewayErr 500 . toApiGatewayResponseBody . Text.pack $ "Something went wrong."

    apiGatewayErr statusCode =
      pure . Left . APIGatewayLambdaError . mkApiGatewayResponse statusCode []

    albErr statusCode =
      pure . Left . ALBLambdaError . mkALBResponse statusCode []
