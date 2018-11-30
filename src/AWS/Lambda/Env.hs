module AWS.Lambda.Env where

import           Relude
import           System.Environment

import qualified AWS.Lambda.Error              as Error

data FunctionSettings = FunctionSettings
  { functionName :: String
  , memorySize :: Int
  , version :: String
  , logStream :: String
  , logGroup :: String
  }

data ProviderCapability = ProviderCapability
  { getFunctionSettings :: IO (Either Error.RuntimeError FunctionSettings)
  , getRuntimeApiEndpoint :: IO (Either Error.RuntimeError String)
  }

envConfig :: ProviderCapability
envConfig = ProviderCapability
  { getFunctionSettings   = do
    functionName <- lookupEnv "AWS_LAMBDA_FUNCTION_NAME"
    version      <- lookupEnv "AWS_LAMBDA_FUNCTION_VERSION"
    logStream    <- lookupEnv "AWS_LAMBDA_LOG_STREAM_NAME"
    logGroup     <- lookupEnv "AWS_LAMBDA_LOG_GROUP_NAME"
    memoryStr    <- lookupEnv "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
    let parsedMemory = memoryStr >>= readMaybe
    case parsedMemory of
      Nothing -> do
        err <-
          Error.newRuntimeError
          $  "Could not parse memory value: "
          <> (memoryStr ?: "<NOTHING>")
          <> "\nMemory value from environment is not an 'Int'"
        return $ Left err

      Just (mem :: Int) -> return $ Right $ FunctionSettings
        { functionName = fromMaybe "" functionName
        , version      = fromMaybe "" version
        , logStream    = fromMaybe "" logStream
        , logGroup     = fromMaybe "" logGroup
        , memorySize   = mem
        }
  , getRuntimeApiEndpoint = do
    endpoint <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
    case endpoint of
      Nothing -> do
        err <- Error.newRuntimeError "Could not read endpoint, was it set?"
        return $ Left err
      Just ep -> return $ Right ep
  }
