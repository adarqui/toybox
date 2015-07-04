module Props (prop_reverse) where

prop_reverse :: [Int] -> Bool
prop_reverse xs = (reverse $ reverse xs) == xs
