module TB.Instances.Identity (
  Identity (..)
) where

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  m >>= f = f $ runIdentity m
  return a = Identity a

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))
