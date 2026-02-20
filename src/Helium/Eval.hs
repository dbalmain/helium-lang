module Helium.Eval (eval) where

import Helium.Syntax (Expr (..))

binOp :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
binOp f a b = f <$> eval a <*> eval b

eval :: Expr -> Either String Int
eval expr = case expr of
  (Lit n) -> Right n
  (Neg e) -> negate <$> eval e
  (Add a b) -> binOp (+) a b
  (Sub a b) -> binOp (-) a b
  (Mul a b) -> binOp (*) a b
  (Div a b) -> do
    x <- eval a
    y <- eval b
    if y == 0 then Left "Division by 0" else Right $ x `div` y
