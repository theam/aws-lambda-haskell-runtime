module Aws.Lambda.ThHelpers
  ( pName
  , eName
  , recordQ
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH

-- | Helper for defining names in declarations
-- think of @myValue@ in @myValue = 2@
pName :: Text -> Q Pat
pName = pure . VarP . mkName . Text.unpack

-- | Helper for defining names in expressions
-- think of @myFunction@ in @quux = myFunction 3@
eName :: Text -> Q Exp
eName = pure . VarE . mkName . Text.unpack


-- | Helper for extracting fields of a specified record
-- it expects the constructor name as the first parameter,
-- and the list of fields to bring into scope as second
-- think of @Person@, and @personAge@, @personName@ in
-- @myFunction Person { personAge, personName } = ...@
recordQ :: Text -> [Text] -> Q Pat
recordQ name fields = do
  extractedFields <- traverse fName fields
  pure $ RecP (mkName $ Text.unpack name) extractedFields
 where
  -- | Helper for extracting fields of records
  -- think of @personAge@ in @myFunction Person { personAge = personAge } = ...@
  fName :: Text -> Q FieldPat
  fName n = pure (mkName $ Text.unpack n, VarP $ mkName $ Text.unpack n)
