module Helium.Syntax where

data Expr
  = Lit Int
  | Var String
  | Let String Expr Expr
  | Lam String Expr
  | App Expr Expr
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)
