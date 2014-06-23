import Data.Function.ArrayMemoize
import System.Environment

fib' :: (Int -> Int) -> Int -> Int
fib' _ 0 = 1
fib' _ 1 = 1
fib' rec n = rec (n - 1) + rec (n - 2)

fib :: Int -> Int
fib = arrayMemoFix (0, 1000) fib'

{-
 {-| Memoize a fixed point of a function over a sub domain.
     Similar to 'fix', but over 'arrayMemo', passing a function a memoized
     version of itself. -}
 {-# INLINE arrayMemoFix #-}
 arrayMemoFix :: (Ix a, ArrayMemoizable b) => (a, a) -> ((a -> b) -> (a -> b)) -> a
 arrayMemoFix (l, u) f = memo_f where memo_f = arrayMemo (l, u) (f memo_f)
-}

main :: IO ()
main = do
 print "array-memoize"
 argv <- getArgs
 case argv of
  (num:[]) -> do
   print $ fib (read num :: Int)
  _ -> print "usage: ./fib <num>"
