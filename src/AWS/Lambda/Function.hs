module Aws.Lambda.Function where

import Relude

import qualified Options.Generic as Options

import AWS.Lambda.Runtime

data LambdaOptions = LambdaOptions
  { eventObject :: Text
  , contextObject :: Context
  } deriving (Generic)
instance Options.ParseRecord LambdaOptions