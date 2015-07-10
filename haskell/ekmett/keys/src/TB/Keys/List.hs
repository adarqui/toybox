module TB.Keys.List (
  module Data.Key
) where

import           Data.Key
import           Prelude  hiding (index, lookup, zip, zipWith)

-- List examples

-- | zip
--
-- >>> zip [1,2,3] [4,5,6]
-- [(1,4),(2,5),(3,6)]


-- | zipWith
--
-- >>> zipWith (+) [1,2,3] [4,5,6]
-- [5,7,9]


-- | zipWithKey
--
-- >>> zipWithKey (\k a b -> (k, a + b)) [1,2,3] [4,5,6]
-- [(0,5),(1,7),(2,9)]


-- | mapWithKey
--
-- >>> mapWithKey (\k a -> (k, a+1)) [1,2,3]
-- [(0,2),(1,3),(2,4)]


-- | foldrWithKey
--
-- >>> foldrWithKey (\k a acc -> acc+k) 0 [1,2,3]
-- 3


-- | traverseWithKey
--
-- >>> traverseWithKey (\k a -> Just (k, a)) [1,2,3]
-- Just [(0,1),(1,2),(2,3)]


-- | index
--
-- >>> index [1,2,3] 0
-- 1


-- | lookup
--
-- >>> lookup 0 [1,2,3]
-- Just 1


-- | adjust
--
-- >>> adjust (+1) 0 [1,2,3]
-- [2,2,3]
--
-- >>> adjust (+1) 1 [1,2,3]
-- [1,3,3]
--
-- >>> adjust (+1) 2 [1,2,3]
-- [1,2,4]
