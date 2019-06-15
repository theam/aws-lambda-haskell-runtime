module Aws.Lambda.Runtime.API.Endpoints
  ( response
  , invocationError
  , runtimeInitError
  , nextInvocation
  , Endpoint(..)
  ) where

import qualified Aws.Lambda.Runtime.API.Version as Version

newtype Endpoint =
  Endpoint String
  deriving (Show)

-- | Endpoint that provides the ID of the next invocation
nextInvocation :: String -> Endpoint
nextInvocation lambdaApi =
  Endpoint $ concat
    [ "http://"
    , lambdaApi
    , "/"
    , Version.value
    , "/runtime/invocation/next"
    ]

-- | Where the response of the Lambda gets published
response :: String -> String -> Endpoint
response lambdaApi requestId =
  Endpoint $ concat
    [ "http://"
    , lambdaApi
    , "/"
    , Version.value
    , "/runtime/invocation/"
    , requestId
    , "/response"
    ]

-- | Invocation (runtime) errors should be published here
invocationError :: String -> String -> Endpoint
invocationError lambdaApi requestId =
  Endpoint $ concat
    [ "http://"
    , lambdaApi
    , "/"
    , Version.value
    , "/runtime/invocation/"
    , requestId
    , "/error"
    ]

-- | Runtime initialization errors should go here
runtimeInitError :: String -> Endpoint
runtimeInitError lambdaApi =
  Endpoint $ concat
    [ "http://"
    , lambdaApi
    , "/"
    , Version.value
    , "/runtime/init/error"
    ]
