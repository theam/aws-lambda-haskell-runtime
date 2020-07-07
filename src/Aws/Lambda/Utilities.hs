module Aws.Lambda.Utilities (toJSONText) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Text
import qualified Data.Text.Encoding as TE

toJSONText :: ToJSON a => a -> Text
toJSONText = TE.decodeUtf8 . LazyByteString.toStrict . encode
