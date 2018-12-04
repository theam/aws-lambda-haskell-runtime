module Aws.Lambda.Function where

import Relude

import qualified Options.Generic as Options
import Language.Haskell.TH

data LambdaOptions = LambdaOptions
  { eventObject     :: Text
  , contextObject   :: Text
  , functionHandler :: Text
  } deriving (Generic)
instance Options.ParseRecord LambdaOptions


mkMain :: Q [Dec]
mkMain = do
  let mainName = mkName "main"
  body <- [e|putStrLn "hi, im generated"|]
  return [ FunD mainName [Clause [] (NormalB body) [] ]]
