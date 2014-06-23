module Core.Flip (
 flip'
) where

flip' :: a -> b -> (b, a)
flip' a b = (b, a)
