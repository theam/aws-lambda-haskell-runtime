{-| Dispatcher generation -}
module Aws.Lambda.Meta.Dispatch
  ( generate
  , decodeObj
  , encodeObj
  , Runtime.LambdaResult(..)
  ) where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Runtime.Common as Runtime

{-| Helper function that the dispatcher will use to
decode the JSON that comes as an AWS Lambda event into the
appropriate type expected by the handler.
-}
decodeObj :: FromJSON a => String -> a
decodeObj x =
  case (eitherDecode $ LazyByteString.pack x) of
    Left e  -> error e
    Right v -> v

{-| Helper function that the dispatcher will use to
decode the JSON that comes as an AWS Lambda event into the
appropriate type expected by the handler.
-}
encodeObj :: ToJSON a => a -> String
encodeObj x = LazyByteString.unpack (encode x)


{-| Generates the dispatcher out of a list of
handler names in the form @src/Foo/Bar.handler@

This dispatcher has a case for each of the handlers that calls
the appropriate qualified function. In the case of the example above,
the dispatcher will call @Foo.Bar.handler@.
-}
generate :: [Text] -> Meta.ExpQ
generate handlerNames = do
  caseExp <- expressionName "functionHandler"
  matches <- traverse handlerCase handlerNames
  unmatched <- unmatchedCase
  pure $ Meta.CaseE caseExp (matches <> [unmatched])

handlerCase :: Text -> Meta.MatchQ
handlerCase lambdaHandler = do
  let pat = Meta.LitP (Meta.StringL $ Text.unpack lambdaHandler)
  body <- [e|do
    result <- $(expressionName qualifiedName) (decodeObj $(expressionName "eventObject")) (decodeObj $(expressionName "contextObject"))
    either (pure . Left . encodeObj) (pure . Right . $(constructorName "LambdaResult") . encodeObj) result |]
  pure $ Meta.Match pat (Meta.NormalB body) []
 where
  qualifiedName =
    lambdaHandler
    & Text.dropWhile (/= '/')
    & Text.drop 1
    & Text.replace "/" "."

unmatchedCase :: Meta.MatchQ
unmatchedCase = do
  let pattern = Meta.WildP
  body <- [e|
    pure $ Left ("Handler " <> $(expressionName "functionHandler") <> " does not exist on project")
    |]
  pure $ Meta.Match pattern (Meta.NormalB body) []
