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

import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import Data.Function ((&))
import Language.Haskell.TH
import qualified Options.Generic as Options
import qualified Data.Conduit as Conduit
import qualified System.Directory as Directory
import System.FilePath ((</>))
import System.IO.Error
import System.IO (hFlush, stdout, stderr)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad.Trans
import Control.Monad
import qualified Data.Text.Encoding    as Encoding
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Void



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
  let pattern = LitP (StringL $ Text.unpack lambdaHandler)
  body <- [e|do
    result <- $(eName qualifiedName) (decodeObj $(eName "eventObject")) (decodeObj $(eName "contextObject"))
    either (returnAndFail $(eName "executionUuid")) (returnAndSucceed $(eName "executionUuid")) result
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
    Left e -> error e
    Right v -> v


data DirContent = DirList [FilePath] [FilePath]
                | DirError IOError
data DirData = DirData FilePath DirContent


-- Produces directory data
walk :: FilePath -> Conduit.ConduitM () DirData IO ()
walk path = do
  result <- lift $ tryIOError listdir
  case result of
    Right dl@(DirList subdirs _) -> do
      Conduit.yield (DirData path dl)
      forM_ subdirs (walk . (path </>))
    Right e -> Conduit.yield (DirData path e)
    Left e -> Conduit.yield (DirData path (DirError e))
 where
  listdir = do
    entries <- filterHidden <$> Directory.getDirectoryContents path
    subdirs <- filterM isDir entries
    files   <- filterM isFile entries
    return $ DirList subdirs files
   where
    isFile entry = Directory.doesFileExist (path </> entry)
    isDir entry = Directory.doesDirectoryExist (path </> entry)
    filterHidden paths = filter (\p -> head p /= '.' && p /= "node_modules") paths


-- Consume directories
myVisitor :: Conduit.ConduitM DirData Void IO [FilePath]
myVisitor = loop []
 where
  loop n = do
    r <- Conduit.await
    case r of
      Nothing -> return n
      Just result  -> loop (process result <> n)
  process (DirData _ (DirError _)) = []
  process (DirData dir (DirList _ files)) = map (\f -> dir <> "/" <> f) files


getHandlers :: IO [Text]
getHandlers = do
  files <- Conduit.runConduit $ walk "." Conduit..| myVisitor
  handlerFiles <- files
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