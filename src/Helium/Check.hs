module Helium.Check (typecheck) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helium.Syntax (Expr (..), Type (..))

type TEnv = Map String Type

expect :: Type -> Type -> Either String ()
expect expected actual
  | expected == actual = Right ()
  | otherwise = Left $ "Expected " <> show expected <> ", got " <> show actual

check :: TEnv -> Expr -> Either String Type
check env expr = case expr of
  Lit _ -> Right TInt
  Var name -> maybe (Left $ "Unbound variable: " <> name) Right (Map.lookup name env)

  Let name def body -> do
    defTy <- check env def
    check (Map.insert name defTy env) body

  Lam param ty body -> do
    bodyTy <- check (Map.insert param ty env) body
    Right $ TFun ty bodyTy

  App f arg -> do
    fTy <- check env f
    case fTy of
      TFun paramTy retTy -> do
        argTy <- check env arg
        expect paramTy argTy
        Right retTy
      _ -> Left $ "Expected function type, got " <> show fTy

  Neg a -> do
    check env a >>= expect TInt
    Right TInt

  Add a b -> checkBinOp env a b
  Sub a b -> checkBinOp env a b
  Mul a b -> checkBinOp env a b
  Div a b -> checkBinOp env a b

checkBinOp :: TEnv -> Expr -> Expr -> Either String Type
checkBinOp env a b = do
  check env a >>= expect TInt
  check env b >>= expect TInt
  Right TInt

typecheck :: Expr -> Either String Type
typecheck = check Map.empty
