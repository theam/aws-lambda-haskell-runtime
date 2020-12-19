module Aws.Lambda.Utilities (toJSONText, tshow) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Text
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

toJSONText :: ToJSON a => a -> Text
toJSONText = T.decodeUtf8 . LazyByteString.toStrict . encode

tshow :: Show a => a -> Text
tshow = T.pack .  show