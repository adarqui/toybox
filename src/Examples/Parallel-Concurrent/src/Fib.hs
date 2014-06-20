{-# LANGUAGE TypeOperators #-}

import System.Environment
import System.Time
import Control.Parallel

{-
par :: a -> b -> b
par _ b = b

pseq :: a -> b -> b
pseq = seq
-}

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mkList :: Int -> [Int]
mkList n = [1..n-1]

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

euler :: Int -> Int
euler n = length (filter (relprime n) (mkList n))

{- The function that we wish to parallelize adds the results of calling ﬁb and sumEuler: -}
sumEuler :: Int -> Int
sumEuler = sum . (map euler) . mkList

{- As a ﬁrst attempt we can try to use par the speculatively spark oﬀ the computation of ﬁb while the parent thread works on sumEuler: -}
sumFibEuler :: Int -> Int -> Int
sumFibEuler a b = fib a + sumEuler b

parSumFibEuler :: Int -> Int -> Int
parSumFibEuler a b = f `par` (f + e)
 where
  f = fib a
  e = sumEuler b

parSumFibEuler' :: Int -> Int -> Int
parSumFibEuler' a b = f `par` (e + f)
 where
  f = fib a
  e = sumEuler b

parSumFibEuler'' :: Int -> Int -> Int
parSumFibEuler'' a b = f `par` (e `pseq` (e + f))
 where
  f = fib a
  e = sumEuler b

{-
To help measure how long a particular computation is taking we use the Sytem.Time
module and deﬁne a function that returns the diﬀerence between two time samples as a number of seconds:
-}
secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

runIt :: String -> (Int -> Int -> Int) -> Int -> Int -> IO ()
runIt desc fn a b = do
 putStrLn $ "Starting: " ++ show desc
 t0 <- getClockTime
 seq (fn a b) (return ())
 t1 <- getClockTime
 putStrLn $ "\tsum: " ++ show (fn a b)
 putStrLn $ "\ttime: " ++ show (secDiff t0 t1) ++ " seconds"

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (f:e:[]) -> do
   runIt "sumFibEuler" sumFibEuler f' e'
   runIt "parSumFibEuler" parSumFibEuler f' e'
   runIt "parSumFibEuler'" parSumFibEuler' f' e'
   runIt "parSumFibEuler''" parSumFibEuler'' f' e'
   where
    f' = read f :: Int
    e' = read e :: Int
  _ -> print "usage: ./Fib <fib> <euler>"
