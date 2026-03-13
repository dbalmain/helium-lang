module Helium.Eval (eval) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helium.Syntax (Expr (..))

type Env = Map String Val

data Val
  = VInt Int
  | VClosure String Expr Env
  deriving (Show)

asInt :: Val -> Either String Int
asInt (VInt x) = Right x
asInt val = Left $ "Expected integer, got " <> show val

binOp :: Env -> (Int -> Int -> Int) -> Expr -> Expr -> Either String Val
binOp env op a b = do
  a' <- eval env a >>= asInt
  b' <- eval env b >>= asInt
  Right . VInt $ op a' b'

eval :: Env -> Expr -> Either String Val
eval env expr = case expr of
  Lit x -> Right $ VInt x
  Var name -> maybe (Left $ "Unbound variable: " <> name) Right (Map.lookup name env)
  Let name definition body -> do
    x <- eval env definition
    eval (Map.insert name x env) body
  Lam parameter body -> Right $ VClosure parameter body env
  App fExpr argExpr -> do
    f <- eval env fExpr
    case f of
      VClosure param body closureEnv -> do
        arg <- eval env argExpr
        eval (Map.insert param arg closureEnv) body
      val -> Left $ "Expected closure, got " <> show val
  Neg a -> VInt . negate <$> (eval env a >>= asInt)
  Add a b -> binOp env (+) a b
  Sub a b -> binOp env (-) a b
  Mul a b -> binOp env (*) a b
  Div a b -> do
    x <- eval env a >>= asInt
    y <- eval env b >>= asInt
    if y == 0 then Left "Division by 0" else Right . VInt $ x `div` y
