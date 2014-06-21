module AFP08.FibEuler (
 fib'euler'sum,
 par'fib'euler'sum'1,
 par'fib'euler'sum'2,
 par'fib'euler'sum'3,
 par'fib'euler'sum'4,
 par'fib'euler'map'1,
 par'fib'euler'map'2
) where

import AFP08.Misc
 (forceList)

import AFP08.Fib
 (fib, mapFibs)

import AFP08.Euler
 (euler, euler'sum, mapEulers)

import Control.Parallel
 (par, pseq)

fib'euler'sum :: Int -> Int -> Int
fib'euler'sum a b = fib a + euler'sum b

par'fib'euler'sum'1 :: Int -> Int -> Int
par'fib'euler'sum'1 a b = f `par` (f + e)
 where
  f = fib a
  e = euler'sum b

par'fib'euler'sum'2 :: Int -> Int -> Int
par'fib'euler'sum'2 a b = f `par` (e + f)
 where
  f = fib a
  e = euler'sum b

par'fib'euler'sum'3 :: Int -> Int -> Int
par'fib'euler'sum'3 a b = f `par` (e `pseq` (f + e))
 where
  f = fib a
  e = euler'sum b

par'fib'euler'sum'4 :: Int -> Int -> Int
par'fib'euler'sum'4 a b = f `par` (e `pseq` (e + f))
 where
  f = fib a
  e = euler'sum b

par'fib'euler'map'1 :: [Int] -> [Int] -> Int
par'fib'euler'map'1 fibs eulers =
 mfibs `par` (meulers `pseq` (sum mfibs + sum meulers))
 where
  mfibs = mapFibs fibs
  meulers = mapEulers eulers

par'fib'euler'map'2 :: [Int] -> [Int] -> Int
par'fib'euler'map'2 fibs eulers =
 (forceList mfibs) `par` ((forceList meulers) `pseq` (sum mfibs + sum meulers))
 where
  mfibs = mapFibs fibs
  meulers = mapEulers eulers
