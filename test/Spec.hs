import Test.Hspec

main :: IO ()
main = hspec $
  describe "Useless test spec" $ do
    it "runs" $ do
      (1 + 1 :: Int) `shouldBe` (2 :: Int)
