{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Aws.Lambda.Runtime.Common
  ( RunCallback,
    LambdaResult (..),
    LambdaError (..),
    LambdaOptions (..),
    ApiGatewayDispatcherOptions (..),
    HandlerType (..),
    HandlerName (..),
    RawEventObject,
  )
where

import Aws.Lambda.Runtime.APIGateway.Types
  ( ApiGatewayDispatcherOptions (..),
    ApiGatewayResponse,
    ApiGatewayResponseBody,
  )
import Aws.Lambda.Runtime.Context (Context)
import Aws.Lambda.Runtime.StandaloneLambda.Types
  ( StandaloneLambdaResponseBody,
  )
import qualified Data.ByteString.Lazy as Lazy
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Callback that we pass to the dispatcher function
type RunCallback (t :: HandlerType) context =
  LambdaOptions context -> IO (Either (LambdaError t) (LambdaResult t))

newtype HandlerName = HandlerName {unHandlerName :: Text}
  deriving newtype (Eq, Show, Read, Ord, Hashable)

data HandlerType
  = StandaloneHandlerType
  | APIGatewayHandlerType

-- | Wrapper type for lambda execution results
data LambdaError (t :: HandlerType) where
  StandaloneLambdaError :: StandaloneLambdaResponseBody -> LambdaError 'StandaloneHandlerType
  APIGatewayLambdaError :: ApiGatewayResponse ApiGatewayResponseBody -> LambdaError 'APIGatewayHandlerType

-- | Wrapper type to handle the result of the user
data LambdaResult (t :: HandlerType) where
  StandaloneLambdaResult :: StandaloneLambdaResponseBody -> LambdaResult 'StandaloneHandlerType
  APIGatewayResult :: ApiGatewayResponse ApiGatewayResponseBody -> LambdaResult 'APIGatewayHandlerType

type RawEventObject = Lazy.ByteString

-- | Options that the generated main expects
data LambdaOptions context = LambdaOptions
  { eventObject :: !RawEventObject,
    functionHandler :: !HandlerName,
    executionUuid :: !Text,
    contextObject :: !(Context context)
  }
  deriving (Generic)
