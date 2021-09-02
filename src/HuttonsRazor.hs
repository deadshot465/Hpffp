module HuttonsRazor where

import Prelude

data Expr
  = Lit Integer
  | Add Expr Expr

instance Show Expr where
  show (Lit n) = show n
  show (Add a b) = show a ++ show b

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b