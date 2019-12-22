module Day3b (
  run, 
  minIntersectSteps,
  eatWireUntil
  ) where

import Text.Read
import Data.Maybe
import Prelude hiding (lookup, update)
import Data.List.Split (splitOn)
import qualified Data.List as DL
import Debug.Trace
import qualified Data.List.Key as DLK
import Control.Parallel.Strategies (parMap, rdeepseq, rpar, rseq)
import Data.Map (Map)
import Data.Graph.Inductive.Query.Monad ((><))

import Lib
import Day3a (
  Point,
  Wire,
  parseProg,
  walkProg,
  findIntersections
  )

eatWireUntil :: Point -> Wire -> (Int, Wire)
eatWireUntil p = (length >< tail) . (span (/= (8,5))) . reverse

-- eatWire :: Wire -> [Point]
eatWire w (p:ps) = (eatWireUntil p) :

-- getDistances :: Wire -> [Point] -> Map Point Int
-- getDistances w((0,0):_) = gd w
--   where gd :: Wire -> [Point] -> Map Point Int
--         gd = undefined


minIntersectSteps a b = ((0,0), 0)
-- minIntersectSteps :: [Wire] -> [Point] -> (Point, Int)
-- minIntersectSteps [a,b] (p:ps) = 
--   let lens = map length [a,b] in
--     map (dropWhile (p /=)) [a,b]

run = do
  input <- aocInputFile "3" >>= readFile
  let progs = map parseProg $ lines input
  let walks = take 2 $ parMap rdeepseq walkProg progs
  let reverseWalks = parMap rdeepseq reverse walks
  let intersections = findIntersections reverseWalks
  -- let counted = minIntersectSteps reverseWalks intersections
  -- putStrLn $ show counted
      
