{-| Helper functions to make code generation easier -}
module Aws.Lambda.Meta.Common
  ( declarationName
  , expressionName
  , getFieldsFrom
  , constructorName
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH

-- | Helper for defining names in declarations
-- think of @myValue@ in @myValue = 2@
declarationName :: Text -> Q Pat
declarationName = pure . VarP . mkName . Text.unpack

-- | Helper for defining names in expressions
-- think of @myFunction@ in @quux = myFunction 3@
expressionName :: Text -> Q Exp
expressionName = pure . VarE . mkName . Text.unpack

-- | Helper for defining names for constructors
-- think of @Foo@ in @quux = Foo 3@
constructorName :: Text -> Q Exp
constructorName = pure . ConE . mkName . Text.unpack

-- | Helper for extracting fields of a specified record
-- it expects the constructor name as the first parameter,
-- and the list of fields to bring into scope as second
-- think of @Person@, and @personAge@, @personName@ in
-- @myFunction Person { personAge, personName } = ...@
getFieldsFrom :: Text -> [Text] -> Q Pat
getFieldsFrom name fields = do
  extractedFields <- traverse extractField fields
  pure $ RecP (mkName $ Text.unpack name) extractedFields
 where
  -- | Helper for extracting fields of records
  -- think of @personAge@ in @myFunction Person { personAge = personAge } = ...@
  extractField :: Text -> Q FieldPat
  extractField n = pure (mkName $ Text.unpack n, VarP $ mkName $ Text.unpack n)
