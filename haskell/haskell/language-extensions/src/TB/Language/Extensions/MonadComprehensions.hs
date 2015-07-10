{-# LANGUAGE MonadComprehensions #-}

module TB.Language.Extensions.MonadComprehensions (
) where

-- need instance of MonadPlus

-- | MonadComprehensions for Maybe
--
-- >>> [ x | x <- Just 123, x > 0 ]
-- Just 123
