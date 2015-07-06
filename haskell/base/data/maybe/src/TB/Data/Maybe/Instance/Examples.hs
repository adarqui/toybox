-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Maybe.html#t:Maybe

module TB.Data.Maybe.Instance.Examples (
) where

-- $setup
-- >>> import Control.Applicative
-- >>> import Control.Monad
-- >>> import Control.Monad.Fix
-- >>> import Control.Monad.Zip
-- >>> import Data.Foldable as F
-- >>> import Data.Monoid
-- >>> import Data.Typeable

-- | Maybe def
-- >>> data Maybe a = Just a | Nothing

-- | Functor examples
--
-- >>> fmap (+1) Nothing
-- Nothing
--
-- >>> fmap (+1) $ Just 1
-- Just 2

-- | Monoid examples
--
-- mempty :: Maybe Int
-- Nothing
--
-- >>> Nothing `mappend` Just [1]
-- Just [1]
--
-- >>> Just [1] `mappend` Nothing
-- Just [1]
--
-- >>> Just [1] `mappend` Just [2]
-- Just [1,2]
--
-- >>> Nothing `mappend` Nothing
-- Nothing

-- | Applicative: pure examples
--
-- >>> pure 1 :: Maybe Int
-- Just 1
--
-- >>> pure (+1) <*> Just 1
-- Just 2
--
-- >>> pure (+5) <*> Nothing
-- Nothing
--
-- >>> pure (+) <*> Nothing <*> Just 1
-- Nothing
--
-- >>> pure (+) <*> Just 1 <*> Just 1
-- Just 2

-- | Applicative: fmap examples
--
-- >>> (+1) <$> Just 1
-- Just 2
--
-- >>> (+) <$> Just 1 <*> Just 1
-- Just 2

-- | Alternative examples
--
-- >>> Nothing <|> Just 1
-- Just 1
--
-- >>> Just 1 <|> Nothing
-- Just 1
--
-- >>> Just 1 <|> Just 2
-- Just 1
--
-- >>> Nothing <|> Nothing <|> Just 1
-- Just 1

-- | Foldable examples
--
-- >>> F.foldl (+) 1 $ Just 10
-- 11

-- | Traversable examples
--

-- | Monad examples
--
-- >>> :{
--  do
--    Nothing >> Nothing >> Just 10
-- :}
-- Nothing
--
-- >>> :{
--  do
--    Just 10 >>= Just . (+1)
-- :}
-- Just 11

-- | MonadPlus examples
--
-- >>> :{
--  do
--    Nothing `mplus` Nothing
-- :}
-- Nothing
--
-- >>> :{
--  do
--    Nothing `mplus` Just 1
-- :}
-- Just 1
--
-- >>> :{
--  do
--    Just 1 `mplus` Just 1
-- :}
-- Just 1
--
--
-- >>> :{
--  do
--    mzero `mplus` Just 2
-- :}
-- Just 2
--
-- >>> :{
--  do
--    Just 2 `mplus` mzero
-- :}
-- Just 2

-- | MonadFix examples
--
-- >>> :{
--  do
--    mfix (\x -> Just 5)
-- :}
-- Just 5

-- | MonadZip examples
--
-- - >>> :{
--  do
--    mzipWith (+) (Just 1) (Just 1)
-- :}
-- Just 2

-- | Data.Data examples
--

-- | Typeable examples
--
