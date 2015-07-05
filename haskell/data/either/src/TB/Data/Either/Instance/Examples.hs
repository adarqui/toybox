-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Either.html#t:Either

module TB.Data.Either.Instance.Examples (
) where

-- $setup
-- >>> import Control.Applicative
-- >>> import Control.Monad
-- >>> import Control.Monad.Fix
-- >>> import Data.Foldable as F
-- >>> import Data.Monoid
-- >>> import Data.Typeable
-- >>> import Data.Either

-- | Either def
-- >>> data Either a b = Left a | Right b

-- | Functor examples
--
-- >>> fmap (+1) $ Left "error"
-- Left "error"
--
-- >>> fmap (+1) $ Right 1
-- Right 2

-- | Monoid examples
--

-- | Applicative: pure examples
--
-- >>> pure 1 :: Either String Int
-- Right 1
--
-- >>> Left "error" <*> Right 1
-- Left "error"
--
-- >>> Right (+1) <*> Right 1
-- Right 2
--
-- >>> Right (+) <*> Right 1 <*> Right 1
-- Right 2

-- | Applicative: fmap examples
--
-- >>> (+) <$> Right 1 <*> Right 1
-- Right 2
--
-- >>> (+) <$> Left "error" <*> Right 1
-- Left "error"
--
-- >>> (+) <$> Right 1 <*> Left "error"
-- Left "error"
--
-- >>> (((+) .) . (+)) <$> Right 1 <*> Right 1 <*> Right 1
-- Right 3

-- | Alternative examples
--

-- | Foldable examples
--

-- | Traversable examples
--

-- | Monad examples
--
-- >>> :{
--  do
--    Right 1 >> Right 2 >> Right 3
-- :}
-- Right 3
--
-- >>> :{
--  do
--    Right 1 >> Left "error" >> Right 3
-- :}
-- Left "error"
--
-- >>> :{
--  do
--    a <- Right 1
--    b <- Left "error"
--    c <- Right 1
--    return (a + b)
-- :}
-- Left "error"
--
-- >>> :{
--  do
--    a <- Right 1
--    b <- Right 1
--    return (a + b)
-- :}
-- Right 2

-- | MonadPlus examples
--

-- | MonadFix examples
--

-- | Data.Data examples
--

-- | Typeable examples
--
