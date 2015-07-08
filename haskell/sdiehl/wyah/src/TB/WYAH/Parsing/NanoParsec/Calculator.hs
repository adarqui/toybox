module TB.WYAH.Parsing.NanoParsec.Calculator (
  Expr (..),
  eval,
  int,
  expr,
  term,
  factor,
  infixOp,
  addOp,
  mulOp,
  run
) where

{-
number = [ ”-” ] digit { digit }.
digit = ”0” | ”1” | ... | ”8” | ”9”.
expr = term { addop term }.
term = factor { mulop factor }.
factor = ”(” expr ”)” | number.
addop = ”+” | ”-”.
mulop = ”*”.
-}

import TB.WYAH.Parsing.NanoParsec

import Control.Applicative
import Control.Monad

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving (Show)

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

run :: String -> Expr
run = runParser expr
