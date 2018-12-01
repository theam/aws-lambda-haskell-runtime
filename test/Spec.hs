import           Relude                  hiding ( head )
import           Relude.Unsafe

import           Test.Hspec
import           Test.QuickCheck


main :: IO ()
main = hspec $ describe "Prelude.head" $ do
  it "returns the first element of a list" $ head [23 ..] `shouldBe` (23 :: Int)

  it "returns the first element of an *arbitrary* list" $ property $ \x xs ->
    head (x : xs) == (x :: Int)

  it "throws an exception if used with an empty list"
    $             evaluateWHNF (head [])
    `shouldThrow` anyException
