module Fib (
 fib1
) where

fib1 :: Int -> Int
fib1 0 = 1
fib1 1 = 1
fib1 n = fib1 (n - 1) + fib1 (n - 2)
