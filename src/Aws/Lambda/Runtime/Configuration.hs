module Aws.Lambda.Runtime.Configuration
  ( DispatcherOptions (..),
    defaultDispatcherOptions,
  )
where

import Aws.Lambda.Runtime.APIGateway.Types (ApiGatewayDispatcherOptions (..))

-- | Options that the dispatcher generator expects
newtype DispatcherOptions = DispatcherOptions
  { apiGatewayDispatcherOptions :: ApiGatewayDispatcherOptions
  }

defaultDispatcherOptions :: DispatcherOptions
defaultDispatcherOptions =
  DispatcherOptions (ApiGatewayDispatcherOptions True)
