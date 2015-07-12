module TB.Semigroups.Examples (
) where

import           Data.Semigroup

newtype Counter = Counter { runCounter :: Integer } deriving (Eq, Show)

instance Semigroup Counter where
  a <> b = Counter $ runCounter a + runCounter b

-- | Counter examples
--
-- >>> Counter 5 <> Counter 5
-- Counter {runCounter = 10}
--
-- >>> times1p 5 (Counter 5)
-- Counter {runCounter = 30}
--
-- >>> Counter 5 <> Counter 5 <> Counter 5 <> Counter 5 <> Counter 5 <> Counter 5 == times1p 5 (Counter 5)
-- True
