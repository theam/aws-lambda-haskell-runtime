{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import Aws.Lambda
import Aws.Lambda.Wai
import Control.Exception (try)
import Control.Monad.Except (ExceptT (..))
import Data.Aeson
import Data.IORef (modifyIORef, readIORef)
import Data.Text as T
import GHC.Generics (Generic)
import Servant hiding (Context)

-- The actual handler.
handler :: WaiHandler AppConfig
handler request context = do

  -- You can access your custom application config using the `customContext` IORef.
  -- This application config will be preserved while the Lambda is warm, so you can reuse
  -- resources like database connections.
  appConfig :: AppConfig <- readIORef $ customContext context

  -- You can also mutate the config by modifying the IORef. On the next Lambda call, you will receive your updated config.
  modifyIORef (customContext context) (\cfg -> let myModifiedCfg = cfg in myModifiedCfg)

  -- This uses Aws.Lambda.Wai from aws-lambda-haskell-runtime-wai in order to convert the Wai application to Lambda.
  waiHandler initializeApplication request context

-- Your application config. This config will be initialized using `initializeAppConfig`
-- once per every cold start. It will be preserved while the lambda is warm.
data AppConfig =
  AppConfig
    { appConfigDbConnection :: DbConnection }

initializeAppConfig :: IO AppConfig
initializeAppConfig = newDbConnection >>= \connection ->
  return $ AppConfig connection

-- We're mocking the DbConnection, but using a real one (e.g. ConnectionPool) will be just as easy.
type DbConnection = ()

newDbConnection :: IO DbConnection
newDbConnection = return ()

data SomeType =
  SomeType
    { aField       :: Int
    , anotherField :: Text
    } deriving (Generic, ToJSON, FromJSON)

-- A mock API returning different content types so you can test around.
type API =
  "html" :> Get '[PlainText] (Headers '[Header "Content-Type" Text] Text) :<|>
  "json" :> Get '[JSON] SomeType :<|>
  "plain" :> Get '[PlainText] Text :<|>
  "empty" :> Get '[PlainText] Text :<|>
  "post" :> ReqBody '[JSON] SomeType :> Post '[JSON] SomeType :<|>
  "error" :> Get '[JSON] SomeType

server :: ServerT API IO
server =
  htmlHandler :<|>
  jsonHandler :<|>
  plainHandler :<|>
  emptyHandler :<|>
  postTest :<|>
  errorHandler
  where
    htmlHandler =
      return $ addHeader @"Content-Type" "text/html" "<html><h1></h1></html>"
    jsonHandler =
      return $ SomeType 1203 "a field"
    plainHandler =
      return "plain text"
    emptyHandler =
      return ""
    postTest =
      return
    errorHandler =
      error "I blew up"

-- Wai application initialization logic
initializeApplication :: IO Application
initializeApplication = return $
  serve (Proxy @API) hoistedServer
  where
    hoistedServer = hoistServer (Proxy @API) ioToHandler server

ioToHandler
  :: IO a
  -> Handler a
ioToHandler = Handler . ExceptT . try
