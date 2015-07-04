module Common.Sums (
 sumFoldl,
 sumFoldl',
 sumFoldr
) where

import Data.List (foldl', foldr)

-- fold/sum --
sumFoldl :: (Num a) => [a] -> a
sumFoldl xs = foldl (+) 0 xs

sumFoldl' ::  (Num a) => [a] -> a
sumFoldl' xs = foldl' (+) 0 xs

sumFoldr ::  (Num a) => [a] -> a
sumFoldr xs = foldr (+) 0 xs
