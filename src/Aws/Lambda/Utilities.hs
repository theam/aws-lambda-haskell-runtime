module Aws.Lambda.Utilities
  ( toJSONText,
    tshow,
    decodeObj,
  )
where

import qualified Aws.Lambda.Runtime.Error as Error
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Proxy (..), Typeable, typeRep)

toJSONText :: ToJSON a => a -> Text
toJSONText = T.decodeUtf8 . LazyByteString.toStrict . encode

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Helper function that the dispatcher will use to
-- decode the JSON that comes as an AWS Lambda event into the
-- appropriate type expected by the handler.
decodeObj :: forall a. (FromJSON a, Typeable a) => LazyByteString.ByteString -> Either Error.Parsing a
decodeObj x =
  let objName = pack . show $ typeRep (Proxy :: Proxy a)
   in case eitherDecode x of
        Left e -> Left $ Error.Parsing (pack e) (pack . LazyByteString.unpack $ x) objName
        Right v -> return v