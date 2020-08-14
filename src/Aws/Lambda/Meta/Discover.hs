{-| Discovery of AWS Lambda handlers
A handler is basically a function that has a type definition that
starts with "handler " and two colons.
 -}
module Aws.Lambda.Meta.Discover
  ( handlers
  ) where

import qualified Control.Monad as Monad
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Path
import qualified Path.IO as PathIO

-- | Paths to ignore during compilation
ignoredPaths :: [Text]
ignoredPaths =
  [ "node_modules"
  , ".stack-work"
  , ".serverless"
  ]

{-| Returns a list of handler paths that look like

@src/Foo/Bar/Quux.handler@

It is the path to the source file, but changing the
extension for ".handler"
-}
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
    fileExtension file == Just ".hs"
    && isNotIgnoredPath file

  isNotIgnoredPath file =
    filter (\ignoredPath -> ignoredPath `Text.isInfixOf` (Text.pack $ toFilePath file)) ignoredPaths
    & null

handlerNames :: [Path Rel File] -> [Text]
handlerNames modules =
  fmap changeExtensionToHandler modules
  & fmap (Text.pack . toFilePath)
 where
  changeExtensionToHandler file =
    replaceExtension ".handler" file
    & Maybe.fromJust  -- The path will be always parsable, as we just replace the extension

containsHandler :: Path Rel File -> IO Bool
containsHandler file = do
  fileContents <- readFile $ toFilePath file
  lines fileContents
    & filter (Text.isPrefixOf "handler :: " . Text.pack)
    & (not . null)
    & pure
