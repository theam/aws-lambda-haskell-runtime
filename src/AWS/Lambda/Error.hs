module AWS.Lambda.Error where

import           Relude
import           System.Environment
import           Debug.Trace.LocationTH

data RuntimeError = RuntimeError
  { runtimeErrorMsg :: String
  , runtimeErrorStackTrace :: Maybe String
  , runtimeErrorRecoverable :: Bool
  , runtimeErrorRequestId :: Maybe String
  } deriving (Show)

data HandlerError = HandlerError
  { handlerErrorMsg :: String
  , handlerErrorStackTrace :: Maybe String
  } deriving (Show)

unrecoverable :: RuntimeError
unrecoverable = RuntimeError
  { runtimeErrorMsg         = ""
  , runtimeErrorStackTrace  = Nothing
  , runtimeErrorRecoverable = False
  , runtimeErrorRequestId   = Nothing
  }

newRuntimeError :: String -> IO RuntimeError
newRuntimeError runtimeErrorMsg = do
  shouldTrace <- lookupEnv "HASKELL_BACKTRACE"
  let runtimeError = RuntimeError
        { runtimeErrorMsg         = runtimeErrorMsg
        , runtimeErrorStackTrace  = Nothing
        , runtimeErrorRecoverable = True
        , runtimeErrorRequestId   = Nothing
        }
  case shouldTrace of
    Just "1" -> do
      let tr = $__LOCATION__
      return $ runtimeError { runtimeErrorStackTrace = Just tr }

    _ -> return runtimeError