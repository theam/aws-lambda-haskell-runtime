{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}
module Aws.Lambda.Configuration
  ( Main.LambdaOptions(..)
  , Main.generate
  , Main.getRecord
  , configureLambda
  , returnAndFail
  , returnAndSucceed
  , decodeObj
  )
where

import Data.Aeson

import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Language.Haskell.TH
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stderr, stdout)

import qualified Aws.Lambda.Meta.Main as Main
import qualified Aws.Lambda.Meta.Run as Run

putTextLn :: Text -> IO ()
putTextLn = putStrLn . Text.unpack

configureLambda :: Q [Dec]
configureLambda = do
  main <- Main.generate
  run <- Run.generate
  return $ main <> [run]

returnAndFail :: ToJSON a => Text -> a -> IO ()
returnAndFail uuid v = do
  hFlush stdout
  putTextLn uuid
  hFlush stdout
  putTextLn (Encoding.decodeUtf8 $ LazyByteString.toStrict $ encode v)
  hFlush stdout
  hFlush stderr
  exitFailure

returnAndSucceed :: ToJSON a => Text -> a -> IO ()
returnAndSucceed uuid v = do
  hFlush stdout
  putTextLn uuid
  hFlush stdout
  putTextLn (Encoding.decodeUtf8 $ LazyByteString.toStrict $ encode v)
  hFlush stdout
  exitSuccess

decodeObj :: FromJSON a => Text -> a
decodeObj x =
  case (eitherDecode $ LazyByteString.fromStrict $ Encoding.encodeUtf8 x) of
    Left e  -> error e
    Right v -> v