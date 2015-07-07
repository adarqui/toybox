module TB.Useless.OneLiners (
  useless01
) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Traversable

-- | useless 01!
--
-- >>> useless 1 2
-- 6
useless01 :: Int -> Int -> Int
useless01 x y = head . catMaybes . fmap (fmap (+1)) $ traverse (:[]) $ fmap (+1) $ (+) <$> Just y <*> (pure y :: Maybe Int)
