module Helium.Eval (eval) where

import Helium.Syntax (Expr (..))

eval :: Expr -> Int
eval expr = case expr of
  (Lit n) -> n
  (Neg e) -> negate (eval e)
  (Add a b) -> eval a + eval b
  (Sub a b) -> eval a - eval b
  (Mul a b) -> eval a * eval b
  (Div a b) -> eval a `div` eval b
