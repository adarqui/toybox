module TB.Data.ArrayMemoize.Fib (
  fibMemo
) where

import           Data.Function.ArrayMemoize
import           System.Environment

fib' :: (Int -> Int) -> Int -> Int
fib' _ 0 = 1
fib' _ 1 = 1
fib' rec n = rec (n - 1) + rec (n - 2)

fibMemo :: Int -> Int
fibMemo = arrayMemoFix (0, 1000) fib'
