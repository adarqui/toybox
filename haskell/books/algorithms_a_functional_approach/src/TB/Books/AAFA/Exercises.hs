{-# LANGUAGE BangPatterns #-}

module TB.Books.AAFA.Exercises (
) where

-- chapter 3

fstf = const
sndf a b = b

-- | t_fab_strictA: seq style
--
-- >>> t_fab_strictA sndf [1..] 0
-- *** Exception: Prelude.undefined
t_fab_strictA :: (b -> t1 -> t) -> b -> t1 -> t
t_fab_strictA f a b  = a `seq` f a b

-- | t_fab_strictB: bang pattern style
--
-- >>> t_fab_strictB fstf 0 undefined
-- *** Exception: Prelude.undefined
--
t_fab_strictB :: (t1 -> b -> t) -> t1 -> b -> t
t_fab_strictB f a !b  = f a b

t_fab_strictAB :: (b -> b1 -> t) -> b -> b1 -> t
t_fab_strictAB f !a !b = f a b


-- | power
--
-- >>> power 2 5
-- 32
power :: (Num a, Integral b) => a -> b -> a
power x k = if (k==0)
            then 1
            else if (k `mod` 2) == 0
              then power (x*x) (k `div` 2)
              else x * (power (x*x) (k `div` 2))
