module Main
  ( main
  ) where

import Control.Monad
import qualified Network.HTTP.Client as Http
import Aws.Lambda.Runtime


httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
  { Http.managerResponseTimeout = Http.responseTimeoutNone
  }

main :: IO ()
main = do
  manager <- Http.newManager httpManagerSettings
  forever (runLambda manager)
