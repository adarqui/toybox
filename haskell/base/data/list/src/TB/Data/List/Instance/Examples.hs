-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-List.html#t:List

module TB.Data.List.Instance.Examples (
) where

-- $setup
-- >>> import Control.Applicative
-- >>> import Control.Monad
-- >>> import Control.Monad.Fix
-- >>> import Control.Monad.Zip
-- >>> import Data.Foldable as F
-- >>> import Data.Monoid
-- >>> import Data.Typeable

-- | Functor examples
--
-- >>> fmap (+1) [1,2,3]
-- [2,3,4]

-- | Monoid examples
--
-- >>> mempty :: [Int]
-- []
--
-- >>> [1,2,3] `mappend` [4,5,6]
-- [1,2,3,4,5,6]
--
-- >>> [1,2,3] `mappend` []
-- [1,2,3]
--
-- >>> [] `mappend` [4,5,6]
-- [4,5,6]

-- | Applicative: pure examples
--
-- >>> pure 1 :: [Int]
-- [1]
--
-- >>> pure (+1) <*> [1,2,3]
-- [2,3,4]
--
-- >>> pure (+) <*> [1,2,3] <*> [4,5,6]
-- [5,6,7,6,7,8,7,8,9]

-- | Applicative: fmap examples
--
-- >>> (+1) <$> [1,2,3]
-- [2,3,4]
--
-- >>> (+) <$> [1,2,3] <*> [4,5,6]
-- [5,6,7,6,7,8,7,8,9]

-- | Alternative examples
--
-- mempty :: [Int]
-- []
--
-- >>> [1,2,3] <|> [4,5,6]
-- [1,2,3,4,5,6]

-- | Foldable examples
--

-- | Traversable examples
--

-- | Monad examples
--
-- >>> :{
--  do
--    [] >> [4,5,6]
-- :}
-- []

-- | MonadPlus examples
--

-- | MonadFix examples
--

-- | MonadZip examples
--

-- | Data.Data examples
--

-- | Typeable examples
--
