module TB.Tagged.Examples (
) where

import Data.Tagged

data SomeData = SomeData {
  d1 :: Bool
} deriving (Show)

class SomeClass g where
  f1 :: g -> Bool
  f2 :: g -> g
  f3 :: Tagged g Int

instance SomeClass SomeData where
  f1 = \g -> not $ d1 g
  f2 = id
  f3 = 0

-- | Tagged example
--
-- >>> f1 (SomeData True)
-- False
--
-- >>> f2 (SomeData True)
-- SomeData {d1 = True}

-- *** This is the Tagged example: ***
--
-- >>> f3 :: Tagged SomeData Int
-- Tagged 0
