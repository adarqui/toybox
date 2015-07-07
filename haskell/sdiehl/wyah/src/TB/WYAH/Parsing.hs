module TB.WYAH.Parsing (
) where

type Name = String

data Expr
  = Var Name
  | Lit Lit
  | Op PrimOp [Expr]
  | Let Name [Name] Expr
  deriving (Show)

data Lit
  = LitInt Int
  deriving (Show)

data PrimOp
  = Add
  deriving (Show)

-- let f x = x + 1
t_fxe1 = Let "f" ["x"] (Op Add [Var "x", Lit (LitInt 1)])

{-
Let ”f” []
  (Lam ”x”
    (TArr TInt TInt)
    (App
      (App
        (Prim ”primAdd”) (Var ”x”))
          (Lit (LitInt 1))))
-}
