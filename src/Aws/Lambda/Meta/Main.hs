module Aws.Lambda.Meta.Main
  ( LambdaOptions(..)
  , generate
  , Options.getRecord
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Options.Generic as Options
import qualified Language.Haskell.TH as Meta

import Aws.Lambda.Meta.Common

data LambdaOptions = LambdaOptions
  { eventObject     :: !Text
  , contextObject   :: !Text
  , functionHandler :: !Text
  , executionUuid   :: !Text
  } deriving (Generic, Options.ParseRecord)

generate :: Meta.DecsQ
generate = [d|
  $(declarationName "main") = getRecord "" >>= run
  |]
