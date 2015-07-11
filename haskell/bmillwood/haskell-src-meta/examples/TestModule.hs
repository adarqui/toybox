module TestModule (
  DataName (..),
  TypeSynonymBool,
  ClassName,
  functionBool
) where

import           Prelude

data DataName dataVariable = DataName dataVariable Bool deriving (Show, Eq)

type TypeSynonymBool = Bool

class ClassName classVariable where
  method :: classVariable

instance ClassName Bool where
  method = True

functionBool :: a -> Bool
functionBool _ = True

functionInt :: a -> Int
functionInt _ = 0

functionId :: a -> a
functionId a = a
