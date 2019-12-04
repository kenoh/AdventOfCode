module Day2aSpec (spec) where

import Test.Hspec
import Day2a

spec = hspec $ do
  describe "handout examples" $ do
    it "works as shown in the instructions" $ do
      intcode2 [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
      intcode2 [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
      intcode2 [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      intcode2 [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
      intcode2 [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]
