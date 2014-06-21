module AFP08.Euler (
 euler,
 euler'sum,
 mapEulers,
 defaultMapEulers
) where

import AFP08.Misc
 (mkList, relprime)

euler :: Int -> Int
euler n = length (filter (relprime n) (mkList n))

euler'sum :: Int -> Int
euler'sum = sum . (map euler) . mkList

mapEulers :: [Int] -> [Int]
mapEulers xs = map euler'sum xs

defaultMapEulers :: [Int]
defaultMapEulers = mapEulers [7600, 7600]
