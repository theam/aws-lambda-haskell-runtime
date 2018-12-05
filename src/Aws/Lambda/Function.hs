module Aws.Lambda.Function
  ( LambdaOptions (..)
  , configureLambda
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
import qualified Data.Text as Text

import Aws.Lambda.ThHelpers

data LambdaOptions = LambdaOptions
  { eventObject     :: Text
  , contextObject   :: Text
  , functionHandler :: Text
  } deriving (Generic)
instance Options.ParseRecord LambdaOptions


mkMain :: Q [Dec]
mkMain = [d|
  $(pName "main") = getRecord "" >>= run
  |]

mkRun :: Q Dec
mkRun = do
  clause' <- recordQ "LambdaOptions" ["functionHandler", "contextObject", "eventObject"]
  body <- dispatcherCaseQ ["src/Lib.handler"]
  pure $ FunD (mkName "run") [Clause [clause'] (NormalB body) []]


dispatcherCaseQ :: [Text] -> Q Exp
dispatcherCaseQ fileNames = do
  caseExp <- eName "functionHandler"
  matches <- traverse handlerCaseQ fileNames
  unmatched <- unmatchedCaseQ
  pure $ CaseE caseExp (matches <> [unmatched])


handlerCaseQ :: Text -> Q Match
handlerCaseQ lambdaHandler = do
  let pattern = LitP (StringL $ toString lambdaHandler)
  body <- [e|do
    result <- $(eName qualifiedName) (decodeObj $(eName "eventObject")) (decodeObj $(eName "contextObject"))
    either returnAndFail returnAndSucceed result
    |]
  pure $ Match pattern (NormalB body) []
 where
  qualifiedName =
    lambdaHandler
    & Text.dropWhile (/= '/')
    & Text.drop 1
    & Text.replace "/" "."


unmatchedCaseQ :: Q Match
unmatchedCaseQ = do
  let pattern = WildP
  body <- [e|
    returnAndFail ("Handler " <> $(eName "functionHandler") <> " does not exist on project")
    |]
  pure $ Match pattern (NormalB body) []

configureLambda :: Q [Dec]
configureLambda = do
  main <- mkMain
  run <- mkRun
  return $ main <> [run]


returnAndFail :: ToJSON a => a -> IO ()
returnAndFail v = do
 putTextLn (decodeUtf8 $ encode v)
 exitFailure

returnAndSucceed :: ToJSON a => a -> IO ()
returnAndSucceed v = do
 putTextLn (decodeUtf8 $ encode v)
 exitSuccess

decodeObj :: FromJSON a => Text -> a
decodeObj x = (decode $ encodeUtf8 x) ?: error $ "Could not decode event " <> x
