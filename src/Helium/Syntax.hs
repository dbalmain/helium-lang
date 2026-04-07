module Helium.Syntax where

import Text.Megaparsec (SourcePos (..), unPos)

data Type
  = TInt
  | TFun Type Type
  deriving (Show, Eq)

data Span = Span SourcePos SourcePos
  deriving (Show, Eq)

data TypeError = TypeError Span String

data Expr
  = Lit Span Int
  | Var Span String
  | Let Span String Expr Expr
  | Lam Span String Type Expr
  | App Span Expr Expr
  | Neg Span Expr
  | Add Span Expr Expr
  | Sub Span Expr Expr
  | Mul Span Expr Expr
  | Div Span Expr Expr
  deriving (Show, Eq)

spanOf :: Expr -> Span
spanOf (Lit s _) = s
spanOf (Var s _) = s
spanOf (Let s _ _ _) = s
spanOf (Lam s _ _ _) = s
spanOf (App s _ _) = s
spanOf (Neg s _) = s
spanOf (Add s _ _) = s
spanOf (Sub s _ _) = s
spanOf (Mul s _ _) = s
spanOf (Div s _ _) = s

spanStart :: Span -> (Int, Int)
spanStart (Span pos _) = (unPos $ sourceLine pos, unPos $ sourceColumn pos)
