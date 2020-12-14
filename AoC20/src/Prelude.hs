-- https://kowainik.github.io/projects/relude#base-noprelude
module Prelude ( module Universum
               , (!!)
               , head'
               , tail'
               , take'
               , every
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