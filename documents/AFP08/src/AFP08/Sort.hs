module AFP08.Sort (
 quicksort,
 quicksort'1
) where

import AFP08.Misc

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = losort ++ x : hisort
 where
  losort = quicksort [ y | y <- xs, y < x ]
  hisort = quicksort [ y | y <- xs, y >= x ]

quicksort'1 :: Int -> Int -> Int -> Int
quicksort'1 x y z = sum $ quicksort input
 where
  input = take z $ randList x y
  seqInput = seq forceList 0
