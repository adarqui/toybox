module Core.Foo (
) where

import Core.ClassFoo

f :: Foo a => a -> [Char]
f x = [foo x, foo x]

g :: Bool -> [Char]
g b = f b
