{-# LANGUAGE FlexibleInstances #-}

module TB.System.Random.Tuples (
  diceRolls
) where

import System.Random

instance Random (Int, Int) where
  randomR ((x1,y1),(x2,y2)) g =
    let
      (x', g1) = randomR (x1, x2) g
      (y', g2) = randomR (y1, y2) g1
    in
      ((x', y'), g2)
  random g = randomR (minBound,maxBound) g

diceRolls :: Int -> [(Int, Int)]
diceRolls = randomRs ((1,1),(6,6)) . mkStdGen
