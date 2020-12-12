module Aws.Lambda.Runtime.API.Endpoints
  ( response,
    invocationError,
    runtimeInitError,
    nextInvocation,
    Endpoint (..),
  )
where

import qualified Aws.Lambda.Runtime.API.Version as Version
import Data.Text (Text)

newtype Endpoint
  = Endpoint Text
  deriving (Show)

-- | Endpoint that provides the ID of the next invocation
nextInvocation :: Text -> Endpoint
nextInvocation lambdaApi =
  Endpoint $
    mconcat
      [ "http://",
        lambdaApi,
        "/",
        Version.value,
        "/runtime/invocation/next"
      ]

-- | Where the response of the Lambda gets published
response :: Text -> Text -> Endpoint
response lambdaApi requestId =
  Endpoint $
    mconcat
      [ "http://",
        lambdaApi,
        "/",
        Version.value,
        "/runtime/invocation/",
        requestId,
        "/response"
      ]

-- | Invocation (runtime) errors should be published here
invocationError :: Text -> Text -> Endpoint
invocationError lambdaApi requestId =
  Endpoint $
    mconcat
      [ "http://",
        lambdaApi,
        "/",
        Version.value,
        "/runtime/invocation/",
        requestId,
        "/error"
      ]

-- | Runtime initialization errors should go here
runtimeInitError :: Text -> Endpoint
runtimeInitError lambdaApi =
  Endpoint $
    mconcat
      [ "http://",
        lambdaApi,
        "/",
        Version.value,
        "/runtime/init/error"
      ]
