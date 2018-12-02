import           Relude                  hiding ( head )

import           Test.Hspec

import           AWS.Lambda.Runtime


handler :: Text -> c -> IO (Either String Int)
handler t _ = do
  putTextLn t
  return $ Right (42 :: Int)

main :: IO ()
main = hspec $ describe "Impure lambda handler" $ do
  it "runs" $ lambda handler
