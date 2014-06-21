module AFP08.Misc (
 mkList,
 forceList,
 relprime,
 randList
) where

import Control.Parallel
 (pseq)

import System.Random
 (randomRs, mkStdGen)

mkList :: Int -> [Int]
mkList n = [1..n-1]

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `pseq` forceList xs

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

randList :: Int -> Int -> [Int]
randList x y = randomRs (x,y) (mkStdGen 42) :: [Int]
