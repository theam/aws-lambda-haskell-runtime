module Aws.Lambda.Meta.Dispatch
  (generate) where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common

generate :: [Text] -> Meta.ExpQ
generate fileNames = do
  caseExp <- expressionName "functionHandler"
  matches <- traverse handlerCase fileNames
  unmatched <- unmatchedCase
  pure $ Meta.CaseE caseExp (matches <> [unmatched])


handlerCase :: Text -> Meta.MatchQ
handlerCase lambdaHandler = do
  let pat = Meta.LitP (Meta.StringL $ Text.unpack lambdaHandler)
  body <- [e|do
    result <- $(expressionName qualifiedName) (decodeObj $(expressionName "eventObject")) (decodeObj $(expressionName "contextObject"))
    either (returnAndFail $(expressionName "executionUuid")) (returnAndSucceed $(expressionName "executionUuid")) result |]
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
    returnAndFail $(expressionName "executionUuid") ("Handler " <> $(expressionName "functionHandler") <> " does not exist on project")
    |]
  pure $ Meta.Match pattern (Meta.NormalB body) []
