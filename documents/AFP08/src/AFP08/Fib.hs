module AFP08.Fib (
 fib,
 mapFibs,
 defaultMapFibs
) where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mapFibs :: [Int] -> [Int]
mapFibs xs = map fib xs

defaultMapFibs :: [Int]
defaultMapFibs = mapFibs [37, 38, 39, 40]
