module TB.QuickCheck.Basic (
) where

import Test.QuickCheck

-- | Basic reverse property testing
--
-- >>> quickCheck propReverse
-- True
propReverse :: [Int] -> Bool
propReverse xs = (reverse . reverse xs) == xs
