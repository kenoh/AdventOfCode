module Day3a (
  run, 
  parseProg, 
  walkProg,
  findIntersections, 
  findShortestDistance,
  walkDirection,
  Point,
  Wire
  ) where

import Text.Read
import Data.Maybe
import Prelude hiding (lookup, update)
import Data.List.Split (splitOn)
import qualified Data.List as DL
import Debug.Trace
import qualified Data.List.Key as DLK
import Control.Parallel.Strategies (parMap, rdeepseq, rpar, rseq)

import Lib


type Instr = (Char, Int)
type Prog = [Instr]
type Point = (Int, Int)
type Wire = [Point]


walkDirection :: Point -> Char -> Int -> Wire
walkDirection (sx, sy) dir cnt = case dir of
  'R' -> [(sx + c, sy) | c <- cs]
  'U' -> [(sx, sy + c) | c <- cs]
  'L' -> [(sx - c, sy) | c <- cs]
  'D' -> [(sx, sy - c) | c <- cs]
  x -> trace ((show x) ++ "wlk") $ undefined
  where cs = [1..cnt]

walkProg' :: Prog -> Wire -> Wire
walkProg' [] w = w
walkProg' ((dir, cnt):insts) current@(tip:_) = walkProg' insts $ (reverse new) ++ current
  where new = walkDirection tip dir cnt :: Wire

walkProg :: Prog -> Wire
walkProg = flip walkProg' [(0, 0)]

findIntersections :: [Wire] -> [Point]
findIntersections [a,b] = DL.intersect a b
-- findIntersections [a,b] = DL.nub . concat . (parMap rdeepseq fn) $ b
--   where fn x = DL.intersect [x] a
findIntersections x = trace "baaad" $ undefined 

parseProg :: String -> Prog
parseProg = (map parseCode) . (splitOn ",")
  where parseCode (direction:count) = (direction, fromJust $ readMaybe count) :: (Char, Int)

distance :: Point -> Int
distance (x,y) = case (x,y) of
  (0,0) -> (maxBound :: Int)
  _ -> x + y

findShortestDistance :: [Point] -> Point
findShortestDistance ps = DLK.minimum distance ps

run = do
  input <- aocInputFile "3" >>= readFile
  let progs = map parseProg $ lines input
  let counted = findShortestDistance . findIntersections . (take 2) $ parMap rdeepseq walkProg progs
  putStrLn $ show counted
      
