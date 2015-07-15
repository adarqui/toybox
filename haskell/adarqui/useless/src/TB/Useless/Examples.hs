module TB.Useless.Examples (
  if',
  if'',
  if'''
) where

-- | if'
--
-- >>> if' (1 == 1) "then" "else"
-- "left"
--
-- >>> if' (1 /= 1) "then" "else"
-- "else"
if' :: Bool -> a -> a -> a
if' a b c
  | a == True = b
  | otherwise = c

if'' :: Bool -> a -> a -> a
if'' a b c | a == True = b | otherwise = c

-- | bad
if''' :: Bool -> a -> a -> a
if''' a b _ | a == True = b
