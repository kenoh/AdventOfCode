import Test.Hspec ( hspec, describe, it, shouldBe )
import Lib ( aocInputInts, aocInput )

import D01a ( d01a )
import D01b ( d01b )
import D02a ( d02a, d02ParseInput )
import D02b ( d02b )
import D03a ( d03a )
import D03b ( d03b )
import D04a ( d04a )
import D04b ( d04b )

spec = do
    describe "D01" $ do
        it "passes the example" $ do
            input <- aocInputInts "01-ex"
            d01a input `shouldBe` 514579
            d01b input `shouldBe` 241861950
    describe "D02" $ do
        it "passes the example" $ do
            input <- d02ParseInput =<< aocInput "02-ex"
            d02a input `shouldBe` 2
            d02b input `shouldBe` 1
    describe "D03" $ do
        it "passes the example" $ do
            input <- aocInput "03-ex"
            d03a input `shouldBe` 7
            d03b input `shouldBe` 336
    describe "D04" $ do
        it "passes the example a" $ do
            input <- aocInput "04-ex"
            d04a input `shouldBe` 2
        it "passes invalid ex" $ do
            invalid <- aocInput "04-ex-invalid"
            d04b invalid `shouldBe` 0
        it "passes valid ex" $ do
            valid <- aocInput "04-ex-valid"
            d04b valid `shouldBe` 4


main :: IO ()
main = hspec spec