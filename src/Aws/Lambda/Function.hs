module Aws.Lambda.Function
  ( LambdaOptions (..)
  , foo
  , returnAndFail
  , returnAndSucceed
  , decodeObj
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


mkMain :: Q Dec
mkMain = do
  let mainName = mkName "main"
  body <- [e|getRecord "" >>= run|]
  return $ FunD mainName [Clause [] (NormalB body) []]

mkRun :: Q Dec
mkRun = do
  let functionName = mkName "run"
  let fhName = mkName "functionHandler"
  let coName = mkName "contextObject"
  let eoName = mkName "eventObject"
  let los = mkName "lo"
  let h1 = mkName "Lib.handler"
  let loName = mkName "LambdaOptions"
  let clause' = AsP los $ RecP loName [ (fhName, VarP fhName), (coName, VarP coName), (eoName, VarP eoName)]
  body <- [e|case $(pure $ VarE fhName) of
    "src/Lib.handler" -> $(pure $ VarE h1) (decodeObj $(pure $ VarE eoName)) (decodeObj $(pure $ VarE coName)) >>= either returnAndFail returnAndSucceed
    _ -> returnAndFail ("Handler " <> $(pure $ VarE fhName) <> " does not exist on project")|]
  return $ FunD functionName [Clause [clause'] (NormalB body) []]

foo = do
  main <- mkMain
  run <- mkRun
  return [main, run]


returnAndFail :: ToJSON a => a -> IO ()
returnAndFail v = do
 putTextLn (decodeUtf8 $ encode v)
 exitFailure

returnAndSucceed :: ToJSON a => a -> IO ()
returnAndSucceed v = do
 putTextLn (decodeUtf8 $  encode v)
 exitFailure

decodeObj :: FromJSON a => Text -> a
decodeObj x = (decode $ encodeUtf8 x) ?: error $ "Could not decode event " <> x
