{-# LANGUAGE NamedFieldPuns #-}
module D02b (D02b.run, d02a, d02ParseInput, d02, run') where

import Relude.Unsafe ((!!))

import D02a

run :: IO ()
run = run' d02b

fits :: Line -> Bool
fits (Line from to subj password) =
  (password !! (from - 1) == subj) `xor` (password !! (to - 1) == subj)

d02b = d02 fits