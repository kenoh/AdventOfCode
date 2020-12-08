-- https://kowainik.github.io/projects/relude#base-noprelude
module Prelude ( module Universum
               , (!!)
               , head'
               , tail'
               , every
) where

import Universum

import Universum.Unsafe ((!!))
import qualified Universum.Unsafe as U

head' = U.head
tail' = U.tail

every n xs = case drop (n-1) xs of
    [] -> []
    (y:ys) -> y : every n ys