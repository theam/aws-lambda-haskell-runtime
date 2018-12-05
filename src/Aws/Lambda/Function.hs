module Aws.Lambda.Function
  ( LambdaOptions (..)
  , mkMain
  , returnAndFail
  , returnAndSucceed
  , Options.getRecord
  )
where

import Relude
import Data.Aeson

import qualified Options.Generic as Options
import Language.Haskell.TH

data LambdaOptions = LambdaOptions
  { eventObject     :: Text
  , contextObject   :: Text
  , functionHandler :: Text
  } deriving (Generic)
instance Options.ParseRecord LambdaOptions


mkMain :: Q [Dec]
mkMain = do
  let mainName = mkName "main"
  body <- [e|putStrLn ("hi, im generated" :: String)|]
  return [ FunD mainName [Clause [] (NormalB body) [] ]]


returnAndFail :: ToJSON a => a -> IO ()
returnAndFail v = do
 putTextLn (decodeUtf8 $ encode v)
 exitFailure

returnAndSucceed :: ToJSON a => a -> IO ()
returnAndSucceed v = do
 putTextLn (decodeUtf8 $  encode v)
 exitFailure
