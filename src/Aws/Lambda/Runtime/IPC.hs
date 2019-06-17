{-| Inter-Process Communication

Used for when the user project is called from a layer.

This is used to call the @haskell_lambda@ executable, which is
provided by the user, when they want to use the layer.

This IPC protocol is based on printing an UUID that is
created by the layer, and then the result. So everything that
is printed before the UUID, is considered STDOUT printed by
the lambda, while what comes after the UUID is considered.

In the case that the lambda execution fails, the exit code
won't be 0 (exit-success), so it will use the STDERR.
-}
module Aws.Lambda.Runtime.IPC
  ( invoke
  , returnAndFail
  , returnAndSucceed
  ) where


import Data.Function ((&))
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Process as Process

import Control.Exception.Safe.Checked
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Aws.Lambda.Runtime.Context (Context (..))
import qualified Aws.Lambda.Runtime.Environment as Environment
import qualified Aws.Lambda.Runtime.Error as Error
import Aws.Lambda.Runtime.Result (LambdaResult (..))

-- | Returns the JSON value failing, according to the protocol
returnAndFail :: ToJSON a => String -> a -> IO ()
returnAndFail uuid v = do
  IO.hFlush IO.stdout
  putStrLn uuid
  IO.hFlush IO.stdout
  putStrLn (ByteString.unpack $ encode v)
  IO.hFlush IO.stdout
  IO.hFlush IO.stderr
  Exit.exitFailure

-- | Returns the JSON value succeeding, according to the protocol
returnAndSucceed :: ToJSON a => String -> a -> IO ()
returnAndSucceed uuid v = do
  IO.hFlush IO.stdout
  putStrLn uuid
  IO.hFlush IO.stdout
  putStrLn (ByteString.unpack $ encode v)
  IO.hFlush IO.stdout
  Exit.exitSuccess

-- | Invokes a function defined by the user as the @haskell_lambda@ executable
invoke
  :: Throws Error.Invocation
  => Throws Error.Parsing
  => Throws Error.EnvironmentVariableNotSet
  => ByteString.ByteString
  -> Context
  -> IO LambdaResult
invoke event context = do
  handlerName <- Environment.handlerName
  runningDirectory <- Environment.taskRoot
  let contextJSON = ByteString.unpack $ encode context
  uuid <- UUID.nextRandom
  out <- Process.readProcessWithExitCode (runningDirectory <> "/haskell_lambda")
                [ "--eventObject", ByteString.unpack event
                , "--contextObject", contextJSON
                , "--functionHandler", handlerName
                , "--executionUuid", UUID.toString uuid
                ]
                ""
  case out of
    (Exit.ExitSuccess, stdOut, _) -> do
      res <- getFunctionResult uuid stdOut
      case res of
        Nothing    -> throw (Error.Parsing "parsing result" stdOut)
        Just value -> pure (LambdaResult value)
    (_, stdOut, stdErr)           ->
      if stdErr /= ""
        then throw (Error.Invocation stdErr)
        else do
          res <- getFunctionResult uuid stdOut
          case res of
            Nothing    -> throw (Error.Parsing "parsing error" stdOut)
            Just value -> throw (Error.Invocation value)

getFunctionResult :: UUID.UUID -> String -> IO (Maybe String)
getFunctionResult u stdOut = do
  let out = String.lines stdOut
  let uuid = UUID.toString u
  printAfterUuid uuid out
  returnAfterUuid uuid out
 where
  printAfterUuid uuid out =
    out
    & takeWhile (/= uuid)
    & mapM_ ( \t -> do
      putStrLn t
      IO.hFlush IO.stdout )

  returnAfterUuid uuid out =
    out
    & dropWhile (/= uuid)
    & dropWhile (== uuid)
    & Maybe.listToMaybe
    & pure
