module TB.Contravariant.Examples (
) where

import Data.Functor.Contravariant
import Data.Function

type Char2Int = Op Char Int

-- | useless
--
-- (getOp $ Op (+1)) 2
-- 3

class Contravariant' f where
  contramap' :: (a -> b) -> f b -> f a

{-
instance Contravariant Maybe where
  contramap _ Nothing = Nothing
  contramap f (Just b) = Just (flip f b)
  -}

newtype Cmp a = Cmp (a -> a -> Bool)
test :: (a -> b) -> Cmp b -> Cmp a
test f (Cmp cmpb) = Cmp (\b1 b2 -> cmpb (f b1) (f b2))
