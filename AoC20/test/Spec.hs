import Test.Hspec ( hspec, describe, it, shouldBe )
import Lib ( aocInputInts, aocInput )

import D01a ( d01a )
import D01b ( d01b )
import D02a ( d02a, d02ParseInput )

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


main :: IO ()
main = hspec spec