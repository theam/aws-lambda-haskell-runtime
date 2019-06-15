module Aws.Lambda.Runtime where

import Data.Aeson
import System.Exit (ExitCode (..))
import qualified Data.String as String
import Data.String (String)
import GHC.Generics
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as ByteString
import Text.Read (readMaybe)
import qualified Data.Text.Encoding    as Encoding
import Control.Monad
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))

import qualified Data.CaseInsensitive as CI
import Lens.Micro.Platform hiding ((.=))
import qualified Network.HTTP.Client as Http
import qualified System.Environment as Environment
import qualified System.Process as Process
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.IO (hFlush, stdout)
import Control.Exception.Safe.Checked

import qualified Aws.Lambda.Runtime.Error as RuntimeError
import qualified Aws.Lambda.Runtime.Environment as Environment


type LByteString = LazyByteString.ByteString
type ByteString = ByteString.ByteString


-- extractHeader :: Wreq.Response LByteString -> String -> String
-- extractHeader apiData header =
--   Encoding.decodeUtf8 (apiData ^. Wreq.responseHeader (CI.mk $ Encoding.encodeUtf8 header))


-- extractIntHeader :: Throws RuntimeError.Value => Wreq.Response LByteString -> String -> IO Int
-- extractIntHeader apiData headerName = do
--   let header = extractHeader apiData headerName
--   case readMaybe header of
--     Just value -> pure value
--     Nothing    -> throw (ParseError "deadline" header)


-- extractBody :: Wreq.Response LByteString -> String
-- extractBody apiData =
--   Encoding.decodeUtf8 $ LazyByteString.toStrict (apiData ^. Wreq.responseBody)



initializeContext :: Wreq.Response LByteString -> IO Context
initializeContext apiData = do
  functionName          <- Environment.functionName
  version               <- Environment.functionVersion
  logStream             <- Environment.logStreamName
  logGroup              <- Environment.logGroup
  memoryLimitInMb       <- Environment.functionMemory
  deadline              <- extractIntHeader apiData "Lambda-Runtime-Deadline-Ms"
  let xrayTraceId        = extractHeader apiData "Lambda-Runtime-Trace-Id"
  let awsRequestId       = extractHeader apiData "Lambda-Runtime-Aws-Request-Id"
  let invokedFunctionArn = extractHeader apiData "Lambda-Runtime-Invoked-Function-Arn"
  propagateXRayTrace xrayTraceId
  pure $ Context
    { functionName       = functionName
    , functionVersion    = version
    , logStreamName      = logStream
    , logGroupName       = logGroup
    , memoryLimitInMb    = memoryLimitInMb
    , invokedFunctionArn = invokedFunctionArn
    , xrayTraceId        = xrayTraceId
    , awsRequestId       = awsRequestId
    , deadline           = deadline
    }


getFunctionResult :: UUID.UUID -> String -> IO (Maybe String)
getFunctionResult u stdOut = do
  let out = String.lines stdOut

  out
   & takeWhile (/= uuid)
   & mapM_ ( \t -> do
    liftIO $ putStrLn t
    liftIO $ hFlush stdout)

  out
   & dropWhile (/= uuid)
   & dropWhile (== uuid)
   & listToMaybe
   & return
 where
  uuid = String.pack $ UUID.toString u


invoke :: String -> Context -> IO LambdaResult
invoke event context = do
  handlerName <- Environment.handlerName
  runningDirectory <- Environment.taskRoot
  let contextJSON = Encoding.decodeUtf8 $ LazyByteString.toStrict $ encode context
  uuid <- liftIO UUID.nextRandom
  out <- liftIO $ Process.readProcessWithExitCode (String.unpack runningDirectory <> "/haskell_lambda")
                [ "--eventObject", String.unpack event
                , "--contextObject", String.unpack contextJSON
                , "--functionHandler", String.unpack handlerName
                , "--executionUuid", UUID.toString uuid
                ]
                ""
  case out of
    (ExitSuccess, stdOut, _) -> do
      res <- getFunctionResult uuid (String.pack stdOut)
      case res of
        Nothing -> throwError (ParseError "parsing result" $ String.pack stdOut)
        Just value -> pure (LambdaResult value)
    (_, stdOut, stdErr)           ->
      if stdErr /= ""
        then throwError (InvocationError $ String.pack stdErr)
        else do
          res <- getFunctionResult uuid (String.pack stdOut)
          case res of
            Nothing -> throwError (ParseError "parsing error" $ String.pack stdOut)
            Just value -> throwError (InvocationError value)

newtype LambdaResult =
  LambdaResult Text

publishResult :: Context -> String -> LambdaResult -> IO ()
publishResult Context {..} lambdaApi (LambdaResult result) =
  void $ liftIO $ Wreq.post (responseEndpoint lambdaApi awsRequestId) (Encoding.encodeUtf8 result)


invokeAndPublish :: Context -> String -> String -> IO ()
invokeAndPublish ctx event lambdaApiEndpoint = do
  res <- invoke event ctx
  publishResult ctx lambdaApiEndpoint res


publishError :: Context -> String -> RuntimeError -> IO ()
publishError Context {..} lambdaApiEndpoint (InvocationError err) =
  void (liftIO $ Wreq.post (invocationErrorEndpoint lambdaApiEndpoint awsRequestId) (Encoding.encodeUtf8 err))

publishError Context {..} lambdaApiEndpoint (ParseError t t2) =
  void (liftIO $ Wreq.post (invocationErrorEndpoint lambdaApiEndpoint awsRequestId) (toJSON $ ParseError t t2))

publishError Context {..} lambdaApiEndpoint err =
  void (liftIO $ Wreq.post (runtimeInitErrorEndpoint lambdaApiEndpoint) (toJSON err))


lambdaRunner :: IO ()
lambdaRunner = do
  lambdaApiEndpoint     <- readEnvironmentVariable "AWS_LAMBDA_RUNTIME_API"
  apiData               <- getApiData lambdaApiEndpoint
  let event = extractBody apiData
  ctx <- initializeContext apiData
  invokeAndPublish ctx event lambdaApiEndpoint `catchError` publishError ctx lambdaApiEndpoint
