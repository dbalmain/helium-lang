module Helium.Eval (eval) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helium.Syntax (Expr (..))

type Env = Map String Int

binOp :: Env -> (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
binOp env op a b = op <$> eval env a <*> eval env b

eval :: Env -> Expr -> Either String Int
eval env expr = case expr of
  (Lit x) -> Right x
  (Var name) -> case Map.lookup name env of
    Just x -> Right x
    Nothing -> Left $ "Unbound variable: " <> name
  (Let name definition body) -> do
    x <- eval env definition
    eval (Map.insert name x env) body
  (Neg a) -> negate <$> eval env a
  (Add a b) -> binOp env (+) a b
  (Sub a b) -> binOp env (-) a b
  (Mul a b) -> binOp env (*) a b
  (Div a b) -> do
    x <- eval env a
    y <- eval env b
    if y == 0 then Left "Division by 0" else Right $ x `div` y
