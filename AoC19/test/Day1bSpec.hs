module Day1bSpec (spec) where

import Test.Hspec
import Day1b (fuel)

spec :: Spec
spec = do
    describe "fuel" $ do
        it "fits the examples in the handout" $ do
            fuel 14 `shouldBe` 2
            fuel 1969 `shouldBe` 966
            fuel 100756 `shouldBe` 50346