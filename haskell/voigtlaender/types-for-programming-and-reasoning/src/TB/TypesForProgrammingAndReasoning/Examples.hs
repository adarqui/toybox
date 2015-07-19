{-# LANGUAGE RankNTypes #-}

module TB.TypesForProgrammingAndReasoning.Examples (
  Fix,
  FList (..),
  FTree (..)
) where

newtype Fix o = Wrap (o (Fix o))

data FList a b = Cons a b | Nil
  deriving (Eq, Show)

instance Functor (FList a) where
  fmap g (Cons a b) = Cons a (g b)
  fmap _ Nil        = Nil

data FTree a b = N b b | L a
  deriving (Eq, Show)

instance Functor (FTree a) where
  fmap g (N b1 b2) = N (g b1) (g b2)
  fmap g (L a)     = L a

cata :: forall f. Functor f => (forall b. (f b -> b) -> Fix f -> b)
cata alg (Wrap t) = alg (fmap (cata alg) t)

t_flist = Wrap (Cons 1 (Wrap (Cons 2 (Wrap (Cons 3 (Wrap Nil)))))) :: Fix (FList Int)
t_ftree = Wrap (N (Wrap (N (Wrap (L 'a')) (Wrap (L 'b')))) (Wrap (L 'c'))) :: Fix (FTree Char)
