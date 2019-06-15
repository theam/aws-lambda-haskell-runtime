module Aws.Lambda.Runtime.IPC
  ( invoke
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
