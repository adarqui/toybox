module TB.Transformers.Data.Functor.Examples (
  mutation
) where

import           Data.Foldable
import           Data.Functor.Classes
import           Data.Functor.Compose
import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum

-- | mutation in the identity monad
--
-- >>> mutation 5
-- 10
mutation :: Num a => a -> a
mutation n = runIdentity $ do
  v <- return $ n + 1
  w <- return $ v + 1
  x <- return $ w + 1
  y <- return $ x + 1
  z <- return $ y + 1
  return z

-- | compose
--
-- >>> (getCompose $ Compose (+)) 1 2
-- 3

-- | constant
--
-- >>> Constant 1
-- Constant 1
--
-- >>> getConstant $ Constant 1
-- 1

-- | Pair
--
-- >>> fmap (+1) $ Pair [1,2,3] [4,5,6]
-- Pair [2,3,4] [5,6,7]
--
-- >>> Pair [(+1)] [(+10)] <*> Pair [1,2] [3,4]
-- Pair [2,3] [13,14]
--
-- >>> Pair (pure (+1)) (pure (+10)) <*> Pair [1,2] [3,4]
-- Pair [2,3] [13,14]
--
-- >>> Pair (Just (+1)) (Just (+10)) <*> Pair (Just 1) (Just 2)
-- Pair (Just 2) (Just 12)

-- | Reverse
--
-- >>> Data.Foldable.foldl1 (-) $ Reverse [1,2,3]
-- 0
--
-- >>> Prelude.foldl1 (-) [1,2,3]
-- -4

-- | Sum
--
-- >>> (InL (Just 1)) :: Sum Maybe Maybe Int
-- InL (Just 1)
--
-- >>> (InL (pure 1)) :: Sum Maybe Maybe Int
-- InL (Just 1)
--
-- >>> compare1 (InL (Just 1) :: Num a => Sum Maybe Maybe a) (InL (Just 1) :: Num a => Sum Maybe Maybe a)
-- EQ
--
-- >>> compare1 (InL (Just 1) :: Num a => Sum Maybe Maybe a) (InR (Just 1) :: Num a => Sum Maybe Maybe a)
-- LT
