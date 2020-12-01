module D01Spec (spec) where

import Test.Hspec
import Lib ( aocInputInts )

import D01a ( d01a )
import D01b ( d01b )

spec = do
    describe "that" $ do
        it "passes the example" $ do
            input <- aocInputInts "01-ex"
            d01a input `shouldBe` 514579
            d01b input `shouldBe` 241861950