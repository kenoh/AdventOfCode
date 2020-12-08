{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module D03b (run, d03b) where

import Data.Tuple.Extra (uncurry3)
import Util (count)
import Lib (aocInput)
import D03a (d03)
run :: IO ()
run = do
    input <- aocInput "03"
    putTextLn $ show $ d03b input


d03b :: Text -> Integer
d03b text = product $ map (`d03` text) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
