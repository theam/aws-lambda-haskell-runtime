{-# LANGUAGE RankNTypes #-}
module Aws.Lambda.Runtime.Configuration
  ( DispatcherOptions (..),
    defaultDispatcherOptions,
    ErrorLogger,
    flushOutput
  )
where

import Aws.Lambda.Runtime.APIGateway.Types (ApiGatewayDispatcherOptions (..))
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import Aws.Lambda.Runtime.Context
import Aws.Lambda.Runtime.Error
import System.IO (stderr, hFlush, stdout)

type ErrorLogger = forall context. Context context -> ErrorType -> Text -> IO ()

defaultErrorLogger :: ErrorLogger
defaultErrorLogger Context {awsRequestId=requestId} errorType message = do
  hPutStrLn stderr $ requestId <> "\t" 
                    <> "ERROR" <> "\t" 
                    <> toReadableType errorType <> "\t" 
                    <> message
  flushOutput

-- | Options that the dispatcher generator expects
data DispatcherOptions = DispatcherOptions
  { apiGatewayDispatcherOptions :: ApiGatewayDispatcherOptions,
    errorLogger :: ErrorLogger
  }

defaultDispatcherOptions :: DispatcherOptions
defaultDispatcherOptions =
  DispatcherOptions (ApiGatewayDispatcherOptions True) defaultErrorLogger

-- | Flush standard output ('stdout') and standard error output ('stderr') handlers
flushOutput :: IO ()
flushOutput = do
  hFlush stdout
  hFlush stderr
