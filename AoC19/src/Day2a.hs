module Day2a (run, intcode2, intcode, parseProgram) where

import Text.Read
import Data.Maybe
import Prelude hiding (lookup, update)
import qualified Data.Sequence as DS
import Data.Foldable
import Data.List.Split (splitOn)
import Data.List

import Lib


intcode3 :: Int -> DS.Seq Int -> DS.Seq Int
intcode3 cur prog = 
  let ap = cur + 1
      bp = cur + 2
      resp = cur + 3
      next = cur + 4
      idx = DS.index prog
      operate fn = intcode3 next $ DS.update (idx resp) (fn (idx . idx $ ap) (idx . idx $ bp)) prog :: DS.Seq Int
  in
    case idx cur of
      99 -> prog
      1 -> operate (+)
      2 -> operate (*)
      _ -> undefined

intcode2 :: [Int] -> [Int]
intcode2 = toList . ((intcode3 0) :: DS.Seq Int -> DS.Seq Int) . DS.fromList

intcode :: (Int, Int) -> [Int] -> Int
intcode (a, b) = head . intcode2 . toList . (DS.update 1 a) . (DS.update 2 b) . DS.fromList

getInts :: [String] -> [Int]
getInts = mapMaybe (readMaybe :: String -> Maybe Int)
        
parseProgram :: String -> [Int]
parseProgram = getInts . (splitOn ",")

run = do
  input <- aocInputFile "2a" >>= readFile
  let progs = map parseProgram $ lines input
  let counted = map (intcode (12, 2)) progs
  putStrLn $ show counted
      
