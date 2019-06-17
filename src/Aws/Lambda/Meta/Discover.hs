module Aws.Lambda.Meta.Discover
  (handlers) where

import qualified Control.Monad as Monad
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Path
import qualified Path.IO as PathIO

handlers :: IO [Text]
handlers = do
  (_, files) <- PathIO.listDirRecurRel [reldir|.|]
  handlerFiles <- modulesWithHandler files
  pure (handlerNames handlerFiles)

modulesWithHandler :: [Path Rel File] -> IO [Path Rel File]
modulesWithHandler files =
  filter isHaskellModule files
  & Monad.filterM containsHandler
 where
  isHaskellModule file =
    fileExtension file == ".hs"

handlerNames :: [Path Rel File] -> [Text]
handlerNames modules =
  fmap changeExtensionToHandler modules
  & fmap (Text.pack . toFilePath)
 where
  changeExtensionToHandler file =
    setFileExtension ".handler" file
    & Maybe.fromJust  -- The path will be always parsable, as we just replace the extension

containsHandler :: Path Rel File -> IO Bool
containsHandler file = do
  fileContents <- readFile $ toFilePath file
  pure $ "handler :: " `Text.isInfixOf` Text.pack fileContents
