module Aws.Lambda.Utilities (toJSONText) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Text
import qualified Data.Text as T

toJSONText :: ToJSON a => a -> Text
toJSONText = T.pack . LazyByteString.unpack . encode