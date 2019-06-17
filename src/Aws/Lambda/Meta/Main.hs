module Aws.Lambda.Meta.Main
  ( LambdaOptions(..)
  , generate
  , Options.getRecord
  ) where

import GHC.Generics (Generic)

import qualified Options.Generic as Options
import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common

data LambdaOptions = LambdaOptions
  { eventObject     :: !String
  , contextObject   :: !String
  , functionHandler :: !String
  , executionUuid   :: !String
  } deriving (Generic, Options.ParseRecord)

generate :: Meta.DecsQ
generate = [d|
  $(declarationName "main") = getRecord "" >>= run
  |]
