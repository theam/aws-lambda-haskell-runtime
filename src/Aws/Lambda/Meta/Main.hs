{-| main function generation for interoperation with the layer -}
module Aws.Lambda.Meta.Main
  ( LambdaOptions(..)
  , generate
  , Options.getRecord
  ) where

import GHC.Generics (Generic)

import qualified Language.Haskell.TH as Meta
import qualified Options.Generic as Options

import Aws.Lambda.Meta.Common

-- | Options that the generated main expects
data LambdaOptions = LambdaOptions
  { eventObject     :: !String
  , contextObject   :: !String
  , functionHandler :: !String
  , executionUuid   :: !String
  } deriving (Generic, Options.ParseRecord)

-- | Generate the main function that the layer will call
generate :: Meta.DecsQ
generate = [d|
  $(declarationName "main") = getRecord "" >>= run
  |]
