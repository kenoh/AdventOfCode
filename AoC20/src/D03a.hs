{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module D03a (run, d03a, d03) where

import Util (count)
import Lib (aocInput)

run :: IO ()
run = do
    input <- aocInput "03"
    putTextLn $ show $ d03a input


fn :: Int -> Int -> String -> Char
fn i width l = l !! (i `mod` width)

-- | Rule is an amount of (right, down) move.
type Rule = (Int, Int)

-- So, there is to Integer since later when multiplying it may overflow :(
d03 :: Rule -> Text -> Integer
d03 ~(rgt, dwn) text = toInteger $ count ('#' ==) $ map (uncurry (`fn` width)) $ zip targetCols $ map toString $ targetLines
    where ls = lines text
          width = (length . head') ls
          targetLines = every dwn $ tail' ls
          targetCols = [rgt,(rgt+rgt)..]

d03a = d03 (3, 1)