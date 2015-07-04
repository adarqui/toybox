module TB.Data.Maybe.Instance.Examples (
) where

import Control.Applicative
import Data.Foldable as F

-- | Maybe def
-- >>> data Maybe a = Just a | Nothing
--

-- | Functor examples
--
-- >>> fmap (+1) Nothing
-- Nothing
--
-- >>> fmap (+1) $ Just 1
-- Just 2
--

-- | Applicative: pure examples
--
-- >>> pure (+1) <*> Just 1
-- Just 2
--
-- >>> pure (+5) <*>) Nothing
-- Nothing
--
-- >>> pure (+) <*> Nothing <*> Just 1
-- Nothing
--
-- >>> pure (+) <*> Just 1 <*> Just 1
-- Just 2
--

-- | Applicative: fmap examples
--
-- >>> (+1) <$> Just 1
-- Just 2
--
-- >>> (+) <$> Just 1 <*> Just 1
-- Just 2
--

-- | Foldable examples
--
-- >>> F.foldl (+) 1 $ Just 10
-- 11
--

-- | Traversable examples
--

-- | Monad examples
--
