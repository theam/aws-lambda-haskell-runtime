module Aws.Lambda.Utilities (toJSONText) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Text
import qualified Data.Text.Encoding as T

toJSONText :: ToJSON a => a -> Text
toJSONText = T.decodeUtf8 . LazyByteString.toStrict . encode
