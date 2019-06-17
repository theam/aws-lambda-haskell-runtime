{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}
module Aws.Lambda.Configuration
  ( LambdaOptions (..)
  , configureLambda
  , returnAndFail
  , returnAndSucceed
  , decodeObj
  , Options.getRecord
  )
where

import Data.Aeson

import Control.Monad
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import GHC.Generics
import Language.Haskell.TH
import qualified Options.Generic as Options
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stderr, stdout)

import Path
import qualified Path.IO as PathIO

import Aws.Lambda.ThHelpers

putTextLn :: Text -> IO ()
putTextLn = putStrLn . Text.unpack

data LambdaOptions = LambdaOptions
  { eventObject     :: Text
  , contextObject   :: Text
  , functionHandler :: Text
  , executionUuid   :: Text
  } deriving (Generic)
instance Options.ParseRecord LambdaOptions


-- This function is the reason why we disable the warning on top of the module
mkMain :: Q [Dec]
mkMain = [d|
  $(pName "main") = getRecord "" >>= run
  |]

mkRun :: Q Dec
mkRun = do
  handlers <- runIO getHandlers
  clause' <- recordQ "LambdaOptions" ["functionHandler", "contextObject", "eventObject", "executionUuid"]
  body <- dispatcherCaseQ handlers
  pure $ FunD (mkName "run") [Clause [clause'] (NormalB body) []]


dispatcherCaseQ :: [Text] -> Q Exp
dispatcherCaseQ fileNames = do
  caseExp <- eName "functionHandler"
  matches <- traverse handlerCaseQ fileNames
  unmatched <- unmatchedCaseQ
  pure $ CaseE caseExp (matches <> [unmatched])


handlerCaseQ :: Text -> Q Match
handlerCaseQ lambdaHandler = do
  let pat = LitP (StringL $ Text.unpack lambdaHandler)
  body <- [e|do
    result <- $(eName qualifiedName) (decodeObj $(eName "eventObject")) (decodeObj $(eName "contextObject"))
    either (returnAndFail $(eName "executionUuid")) (returnAndSucceed $(eName "executionUuid")) result |]
  pure $ Match pat (NormalB body) []
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
    returnAndFail $(eName "executionUuid") ("Handler " <> $(eName "functionHandler") <> " does not exist on project")
    |]
  pure $ Match pattern (NormalB body) []

configureLambda :: Q [Dec]
configureLambda = do
  main <- mkMain
  run <- mkRun
  return $ main <> [run]


returnAndFail :: ToJSON a => Text -> a -> IO ()
returnAndFail uuid v = do
  hFlush stdout
  putTextLn uuid
  hFlush stdout
  putTextLn (Encoding.decodeUtf8 $ LazyByteString.toStrict $ encode v)
  hFlush stdout
  hFlush stderr
  exitFailure

returnAndSucceed :: ToJSON a => Text -> a -> IO ()
returnAndSucceed uuid v = do
  hFlush stdout
  putTextLn uuid
  hFlush stdout
  putTextLn (Encoding.decodeUtf8 $ LazyByteString.toStrict $ encode v)
  hFlush stdout
  exitSuccess

decodeObj :: FromJSON a => Text -> a
decodeObj x =
  case (eitherDecode $ LazyByteString.fromStrict $ Encoding.encodeUtf8 x) of
    Left e  -> error e
    Right v -> v

getHandlers :: IO [Text]
getHandlers = do
  (_, files) <- PathIO.listDirRecurRel [reldir|.|]
  handlerFiles <- files
                   & fmap toFilePath
                   & fmap Text.pack
                   & filter (Text.isSuffixOf ".hs")
                   & filterM containsHandler
                   & fmap (fmap $ Text.dropEnd 3)
                   & fmap (fmap $ Text.drop 2)
                   & fmap (fmap (<> ".handler"))
  return handlerFiles


containsHandler :: Text -> IO Bool
containsHandler file = do
  fileContents <- readFile $ Text.unpack file
  return $ "handler :: " `Text.isInfixOf` Text.pack fileContents
