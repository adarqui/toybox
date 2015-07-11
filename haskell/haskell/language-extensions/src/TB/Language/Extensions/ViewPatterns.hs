{-# LANGUAGE ViewPatterns #-}

-- https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns

module TB.Language.Extensions.ViewPatterns (
  Record (..),
  viewRecCChar,
  recTest,
  factorial,
  searchList
) where

import Data.List

-- Factorial example

viewFactorial :: Int -> Maybe Int
viewFactorial n = if n > 0 then Just n else Nothing

factorial :: Int -> Int
factorial (viewFactorial -> Just n) = n * factorial (n-1)
factorial _ = 1


-- List/Map lookup example

searchList :: Eq a => a -> [a] -> Either String a
searchList a (find (== a) -> Just x) = Right x
searchList _ _ = Left "not found"


-- Record

data Record
  = RecA Int
  | RecB Bool Bool
  | RecC Int Bool Char
  deriving (Show)

viewRecCChar :: Record -> Maybe Char
viewRecCChar (RecC _ _ c) = Just c
viewRecCChar _ = Nothing

recTest :: Record -> (Char, String)
recTest (viewRecCChar -> Just c) = (c, "boom")
recTest _ = ('!', "bang")
