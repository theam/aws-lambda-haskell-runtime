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
import Relude

import qualified Data.Text as Text
import Language.Haskell.TH
import qualified Options.Generic as Options
import qualified Conduit as Conduit
import qualified System.Directory as Directory
import System.FilePath ((</>))
import System.IO.Error

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
  handlers <- runIO getHandlers
  clause' <- recordQ "LambdaOptions" ["functionHandler", "contextObject", "eventObject"]
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

data DirContent = DirList [FilePath] [FilePath]
                | DirError IOError
data DirData = DirData FilePath DirContent

-- Produces directory data
walk :: FilePath -> Conduit.Source IO DirData
walk path = do
  result <- lift $ tryIOError listdir
  case result of
    Right dl@(DirList subdirs files) -> do
      Conduit.yield (DirData path dl)
      forM_ subdirs (walk . (path </>))
    Right e -> Conduit.yield (DirData path e)
    Left e -> Conduit.yield (DirData path (DirError e))
 where
  listdir = do
    entries <- Directory.getDirectoryContents path >>= filterHidden
    subdirs <- filterM isDir entries
    files   <- filterM isFile entries
    return $ DirList subdirs files
   where
    isFile entry = Directory.doesFileExist (path </> entry)
    isDir entry = Directory.doesDirectoryExist (path </> entry)
    filterHidden paths = return $ filter (\p -> head (fromMaybe (error "") $ nonEmpty p) /= '.') paths


-- Consume directories
myVisitor :: Conduit.Sink DirData IO [FilePath]
myVisitor = loop []
 where
  loop n = do
    r <- Conduit.await
    case r of
      Nothing -> return n
      Just r  -> loop (process r <> n)
  process (DirData _ (DirError _)) = []
  process (DirData dir (DirList _ files)) = map (\f -> dir <> "/" <> f) files


getHandlers :: IO [Text]
getHandlers = do
  files <- Conduit.runConduit $ walk "." Conduit..| myVisitor
  handlerFiles <- files
                   & fmap toText
                   & filter (Text.isSuffixOf ".hs")
                   & filterM containsHandler
                   & fmap (fmap $ Text.dropEnd 3)
                   & fmap (fmap $ Text.drop 2)
                   & fmap (fmap (<> ".handler"))
  return handlerFiles


containsHandler :: Text -> IO Bool
containsHandler file = do
  fileContents <- readFile $ toString file
  return $ "handler :: " `Text.isInfixOf` fileContents
        && "IO (Either " `Text.isInfixOf` fileContents