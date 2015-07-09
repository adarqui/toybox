module TB.Data.Function.Examples (
) where

import           Data.Function

-- | reverse function application
--
-- >>> [1,2,3] ++ [4,5,6] $$ map (+1)
-- [2,3,4,5,6,7]
--
-- -- >>> [1,2,3] ++ [4,5,6] & map (+1)
-- -- [2,3,4,5,6,7]-
--
-- This is the same as "&" in Data.Function for base 4.8
infixr 0 $$
($$) :: a -> (a -> b) -> b
a $$ f = f a
