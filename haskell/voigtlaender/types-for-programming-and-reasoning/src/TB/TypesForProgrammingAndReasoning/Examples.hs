{-# LANGUAGE RankNTypes #-}

module TB.TypesForProgrammingAndReasoning.Examples (
  Fix,
  FList (..),
  FTree (..),
  cata,
  listSum,
  treeSum
) where

newtype Fix o = Wrap (o (Fix o))

data FList a b = Cons a b | Nil
  deriving (Eq, Show)

data FTree a b = N b b | L a
  deriving (Eq, Show)

-- | FList functor
--
-- >>> fmap (id) (Cons 1 2)
-- Cons 1 2
--
-- >>> fmap (+1) (Cons 1 2)
-- Cons 1 3
--
-- >>> fmap id Nil
-- Nil
instance Functor (FList a) where
  fmap g (Cons a b) = Cons a (g b)
  fmap _ Nil        = Nil

-- | FTree functor
--
-- >>> fmap (+1) (L 1)
-- L 1
--
-- >>> fmap (+1) (N 1 2)
-- N 2 3
instance Functor (FTree a) where
  fmap g (N b1 b2) = N (g b1) (g b2)
  fmap g (L a)     = L a

cata :: forall f. Functor f => (forall b. (f b -> b) -> Fix f -> b)
cata alg (Wrap t) = alg (fmap (cata alg) t)

-- | weird cata stuff
--
-- >>> cata (\Nothing -> 1) (Wrap Nothing)
-- 1

-- | listSum
--
-- >>> listSum (Wrap (Cons 1 (Wrap (Cons 2 (Wrap (Cons 3 (Wrap Nil)))))) :: Fix (FList Int))
-- 6
listSum :: Num a => Fix (FList a) -> a
listSum = cata alg
  where
    alg Nil        = 0
    alg (Cons n r) = n + r

-- | treeSum
--
-- >>> treeSum (Wrap (N (Wrap (N (Wrap (L 1)) (Wrap (L 2)))) (Wrap (L 3))) :: Fix (FTree Int))
-- 6
treeSum :: Num a => Fix (FTree a) -> a
treeSum = cata alg
  where
    alg  (L n)    = n
    alg (N b1 b2) = b1 + b2

-- | eitherSum
--
-- >>> eitherSum (Wrap (Left 1))
-- 1
--
-- >>> eitherSum (Wrap (Right (Wrap (Left 2))))
-- 2
eitherSum :: Num a => Fix (Either a) -> a
eitherSum = cata alg
  where
    alg (Left n)  = n
    alg (Right n) = n

t_maybe = Wrap (Just (Wrap Nothing)) :: Fix Maybe
t_flist = Wrap (Cons 1 (Wrap (Cons 2 (Wrap (Cons 3 (Wrap Nil)))))) :: Fix (FList Int)
t_ftree = Wrap (N (Wrap (N (Wrap (L 'a')) (Wrap (L 'b')))) (Wrap (L 'c'))) :: Fix (FTree Char)
