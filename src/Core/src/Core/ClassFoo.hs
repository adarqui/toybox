module Core.ClassFoo (
 Foo(..)
) where

class Foo a where
 foo :: a -> Char

instance Foo Bool where
 foo True = 't'
 foo False = 'f'
