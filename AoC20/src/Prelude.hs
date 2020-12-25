-- https://kowainik.github.io/projects/relude#base-noprelude
module Prelude ( module Universum
               , (!!)
               , head'
               , tail'
               , take'
               , every
               , orFailWith
               , debugFilter
               , filterEither
) where

import Universum

import Universum.Unsafe ((!!))
import qualified Universum.Unsafe as U
import Data.Text as DT

head' = U.head
tail' = U.tail
take' = DT.take

every n xs = case Universum.drop (n-1) xs of
    [] -> []
    (y:ys) -> y : every n ys

--(.:) :: (t1 -> t2) -> (t3 -> t4 -> t1) -> t3 -> t4 -> t2
(f .: g) x y = f (g x y)
infixr 8 .:

orFailWith :: a -> Bool -> Either a Bool
orFailWith e True = Right True
orFailWith e False = Left e

debugFilter :: (Show a, Show b) => (a -> b) -> a -> b 
debugFilter f x = traceShow (x, res) res
    where res = f x

filterEither :: (a -> Either e x) -> [a] -> [a]
filterEither f = Universum.filter (isRight . f)