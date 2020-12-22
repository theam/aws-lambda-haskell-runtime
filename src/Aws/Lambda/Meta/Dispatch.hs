{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Dispatcher generation
module Aws.Lambda.Meta.Dispatch
  ( generate,
    decodeObj,
    Runtime.LambdaResult (..),
  )
where

import Aws.Lambda.Meta.Common
import qualified Aws.Lambda.Meta.Main as Main
import qualified Aws.Lambda.Runtime.ApiGatewayInfo as ApiGatewayInfo
import Aws.Lambda.Runtime.Common (toStandaloneLambdaResponse)
import qualified Aws.Lambda.Runtime.Common as Runtime
import qualified Aws.Lambda.Runtime.Error as Error
import qualified Control.Exception as Unchecked
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.Char as Char
import Data.Function ((&))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Typeable (Proxy (..), Typeable, typeRep)
import qualified Language.Haskell.TH as Meta
import qualified System.IO as IO
import Aws.Lambda.Utilities (tshow)

-- | Helper function that the dispatcher will use to
-- decode the JSON that comes as an AWS Lambda event into the
-- appropriate type expected by the handler.
decodeObj :: forall a. (FromJSON a, Typeable a) => LazyByteString.ByteString -> Either Error.Parsing a
decodeObj x =
  let objName = pack . show $ (typeRep (Proxy :: Proxy a))
   in case (eitherDecode x) of
        Left e -> Left $ Error.Parsing (pack e) (pack . LazyByteString.unpack $ x) objName
        Right v -> return v

-- | Generates the dispatcher out of a list of
-- handler names in the form @src/Foo/Bar.handler@
--
-- This dispatcher has a case for each of the handlers that calls
-- the appropriate qualified function. In the case of the example above,
-- the dispatcher will call @Foo.Bar.handler@.
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
  body <-
    [e|
      do
        case decodeObj $(expressionName "eventObject") of
          Right eventObject ->
            ( do
                result <- $(expressionName (qualifiedHandlerName lambdaHandler)) eventObject contextObject
                either (pure . Left . Runtime.StandaloneLambdaError . toStandaloneLambdaResponse) (pure . Right . Runtime.StandaloneLambdaResult . toStandaloneLambdaResponse) result
            )
              `Unchecked.catch` \(handlerError :: Unchecked.SomeException) -> do
                IO.hPutStr IO.stderr $ show handlerError
                pure . Left . Runtime.StandaloneLambdaError . toStandaloneLambdaResponse . tshow $ handlerError
          Left err -> pure . Left . Runtime.StandaloneLambdaError . toStandaloneLambdaResponse $ err
      |]
  pure $ Meta.Match pat (Meta.NormalB body) []

standaloneLambdaUnmatchedCase :: Meta.MatchQ
standaloneLambdaUnmatchedCase = do
  let pattern = Meta.WildP
  body <-
    [e|
      pure . Left . Runtime.StandaloneLambdaError . toStandaloneLambdaResponse $ ("Handler " <> $(expressionName "functionHandler") <> " does not exist on project" :: Text)
      |]
  pure $ Meta.Match pattern (Meta.NormalB body) []

apiGatewayHandlerCase :: Main.DispatcherOptions -> Text -> Meta.MatchQ
apiGatewayHandlerCase options lambdaHandler = do
  let pat = Meta.LitP (Meta.StringL $ Text.unpack lambdaHandler)
  body <-
    [e|
      do
        let returnErr statusCode = pure . Left . Runtime.APIGatewayLambdaError . ApiGatewayInfo.mkApiGatewayResponse statusCode
        case decodeObj $(expressionName "eventObject") of
          Right eventObject -> do
            resultE <- Unchecked.try $ $(expressionName (qualifiedHandlerName lambdaHandler)) eventObject contextObject
            case resultE of
              Right result ->
                either (pure . Left . Runtime.APIGatewayLambdaError . fmap toApiGatewayResponseBody) (pure . Right . Runtime.APIGatewayResult . fmap toApiGatewayResponseBody) result
              Left (handlerError :: Unchecked.SomeException) -> do
                IO.hPutStr IO.stderr $ show handlerError
                if (Runtime.propagateImpureExceptions . Runtime.apiGatewayDispatcherOptions $ options)
                  then returnErr 500 . toApiGatewayResponseBody . tshow $ handlerError
                  else returnErr 500 . toApiGatewayResponseBody . Text.pack $ "Something went wrong."
          Left err -> returnErr 400 . toApiGatewayResponseBody . tshow $ err
      |]
  pure $ Meta.Match pat (Meta.NormalB body) []

apiGatewayUnmatchedCase :: Meta.MatchQ
apiGatewayUnmatchedCase = do
  let pattern = Meta.WildP
  body <-
    [e|
      pure . Left . Runtime.APIGatewayLambdaError . ApiGatewayInfo.mkApiGatewayResponse 500 . toApiGatewayResponseBody $ ("Handler " <> $(expressionName "functionHandler") <> " does not exist on project")
      |]
  pure $ Meta.Match pattern (Meta.NormalB body) []

qualifiedHandlerName :: Text -> Text
qualifiedHandlerName lambdaHandler =
  lambdaHandler
    & Text.splitOn "/"
    & filter (Char.isUpper . Text.head)
    & Text.intercalate "."
