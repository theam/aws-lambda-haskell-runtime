module Aws.Lambda.API
  () where

import qualified Network.HTTP.Client as Http
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString

type LByteString = LazyByteString.ByteString

getApiData :: String -> IO (Wreq.Response LByteString)
getApiData endpoint =
  keepRetrying (Wreq.getWith opts $ nextInvocationEndpoint endpoint)
 where
  opts = Wreq.defaults
         & Wreq.manager .~ Left (Http.defaultManagerSettings { Http.managerResponseTimeout = Http.responseTimeoutNone })
  keepRetrying :: IO (Wreq.Response LByteString) -> IO (Wreq.Response LByteString)
  keepRetrying f = do
    result <- (liftIO $ try f) :: IO (Either IOException (Wreq.Response LByteString))
    case result of
      Right x -> return x
      _ -> keepRetrying f
