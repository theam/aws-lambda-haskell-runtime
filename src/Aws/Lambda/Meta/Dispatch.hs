{-# LANGUAGE FlexibleContexts #-}

{-| Dispatcher generation -}
module Aws.Lambda.Meta.Dispatch
  ( generate
  , decodeObj
  , encodeObj
  , Runtime.LambdaResult(..)
  ) where

import qualified Data.Char as Char
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Meta.Main as Main
import qualified Aws.Lambda.Runtime.ApiGatewayInfo as ApiGatewayInfo
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Control.Exception as Unchecked
import Data.Typeable (Proxy (..), Typeable, typeRep)

{-| Helper function that the dispatcher will use to
decode the JSON that comes as an AWS Lambda event into the
appropriate type expected by the handler.
-}
decodeObj :: forall a. (FromJSON a, Typeable a) => LazyByteString.ByteString -> Either Error.Parsing a
decodeObj x =
  let objName = show (typeRep (Proxy :: Proxy a)) in
  case (eitherDecode x) of
    Left e  -> Left $ Error.Parsing e (LazyByteString.unpack x) objName
    Right v -> return v

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
generate :: Main.DispatcherOptions -> Main.DispatcherStrategy -> [Text] -> Meta.ExpQ
generate options strategy handlerNames = do
  caseExp <- expressionName "functionHandler"
  case strategy of
    Main.StandaloneLambda -> do
      matches <- traverse standaloneLambdaHandlerCase handlerNames
      unmatched <- standaloneLambdaUnmatchedCase
      pure $ Meta.CaseE caseExp (matches <> [unmatched])
    Main.UseWithAPIGateway -> do
      matches <- traverse (apiGatewayHandlerCase options) handlerNames
      unmatched <- apiGatewayUnmatchedCase
      pure $ Meta.CaseE caseExp (matches <> [unmatched])

standaloneLambdaHandlerCase :: Text -> Meta.MatchQ
standaloneLambdaHandlerCase lambdaHandler = do
  let pat = Meta.LitP (Meta.StringL $ Text.unpack lambdaHandler)
  body <- [e|do
    case decodeObj $(expressionName "eventObject") of
      Right eventObject -> (do
          result <- $(expressionName (qualifiedHandlerName lambdaHandler)) eventObject contextObject
          either (pure . Left . Runtime.StandaloneLambdaError . encodeObj) (pure . Right . Runtime.StandaloneLambdaResult . encodeObj) result)
          `Unchecked.catch` \(handlerError :: Unchecked.SomeException) -> pure . Left . Runtime.StandaloneLambdaError . encodeObj . show $ handlerError
      Left err -> pure . Left . Runtime.StandaloneLambdaError . encodeObj $ err|]
  pure $ Meta.Match pat (Meta.NormalB body) []

standaloneLambdaUnmatchedCase :: Meta.MatchQ
standaloneLambdaUnmatchedCase = do
  let pattern = Meta.WildP
  body <- [e|
    pure . Left . Runtime.StandaloneLambdaError . encodeObj $ ("Handler " <> $(expressionName "functionHandler") <> " does not exist on project")
    |]
  pure $ Meta.Match pattern (Meta.NormalB body) []

apiGatewayHandlerCase :: Main.DispatcherOptions -> Text -> Meta.MatchQ
apiGatewayHandlerCase options lambdaHandler = do
  let pat = Meta.LitP (Meta.StringL $ Text.unpack lambdaHandler)
  body <- [e|do
    let returnErr statusCode = pure . Left . Runtime.ApiGatewayLambdaError . ApiGatewayInfo.mkApiGatewayResponse statusCode
    case decodeObj $(expressionName "eventObject") of
      Right eventObject -> do
        resultE <- Unchecked.try $ $(expressionName (qualifiedHandlerName lambdaHandler)) eventObject contextObject
        case resultE of
          Right result ->
            either (pure . Left . Runtime.ApiGatewayLambdaError . fmap toApiGatewayResponseBody) (pure . Right . Runtime.ApiGatewayResult . fmap toApiGatewayResponseBody) result
          Left (handlerError :: Unchecked.SomeException) ->
            if (Runtime.propagateImpureExceptions . Runtime.apiGatewayDispatcherOptions $ options)
            then returnErr 500 . toApiGatewayResponseBody . show $ handlerError
            else returnErr 500 . toApiGatewayResponseBody . Text.pack $ "Something went wrong."
      Left err -> returnErr 400 . toApiGatewayResponseBody . show $ err|]
  pure $ Meta.Match pat (Meta.NormalB body) []

apiGatewayUnmatchedCase :: Meta.MatchQ
apiGatewayUnmatchedCase = do
  let pattern = Meta.WildP
  body <- [e|
    pure . Left . Runtime.ApiGatewayLambdaError . ApiGatewayInfo.mkApiGatewayResponse 500 . toApiGatewayResponseBody $ ("Handler " <> $(expressionName "functionHandler") <> " does not exist on project")
    |]
  pure $ Meta.Match pattern (Meta.NormalB body) []

qualifiedHandlerName :: Text -> Text
qualifiedHandlerName lambdaHandler =
    lambdaHandler
    & Text.splitOn "/"
    & filter (Char.isUpper . Text.head)
    & Text.intercalate "."
