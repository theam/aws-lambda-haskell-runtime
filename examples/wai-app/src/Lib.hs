{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where

import           Aws.Lambda
import           Aws.Lambda.Wai
import           Control.Exception    (try)
import           Control.Monad.Except (ExceptT (..))
import           Data.Aeson
import           Data.Text            as T
import           GHC.Generics         (Generic)
import           Servant              hiding (Context)

data SomeType =
  SomeType
    { aField       :: Int
    , anotherField :: Text
    } deriving (Generic, ToJSON, FromJSON)

type API =
  "html" :> Get '[PlainText] (Headers '[Header "Content-Type" Text] Text) :<|>
  "json" :> Get '[JSON] (Headers '[Header "Content-Type" Text] SomeType) :<|>
  "plain" :> Get '[PlainText] (Headers '[Header "Content-Type" Text] Text) :<|>
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
      return $ addHeader @"Content-Type" "application/json" (SomeType 1203 "a field")
    plainHandler =
      return $ addHeader @"Content-Type" "text/plain" "plain text"
    emptyHandler =
      return ""
    postTest =
      return
    errorHandler =
      error "I blew up"

initializeApplication :: IO Application
initializeApplication = return $
  serve (Proxy @API) hoistedServer
  where
    hoistedServer = hoistServer (Proxy @API) ioToHandler server

ioToHandler
  :: IO a
  -> Handler a
ioToHandler = Handler . ExceptT . try

handler :: WaiHandler ()
handler = waiHandler initializeApplication
