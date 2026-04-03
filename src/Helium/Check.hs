module Helium.Check (elaborate) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helium.Core (Core (..))
import Helium.Syntax (Expr (..), Type (..))

type TEnv = Map String Type

expect :: Type -> (Type, Core) -> Either String Core
expect expected (actual, core)
  | expected == actual = Right core
  | otherwise = Left $ "Expected " <> show expected <> ", got " <> show actual

elab :: TEnv -> Expr -> Either String (Type, Core)
elab env expr = case expr of
  Lit x -> Right (TInt, CLit x)
  Var name -> case Map.lookup name env of
    Just ty -> Right (ty, CVar name)
    Nothing -> Left $ "Unbound variable: " <> name

  Let name def body -> do
    (defTy, defCore) <- elab env def
    (bodyTy, bodyCore) <- elab (Map.insert name defTy env) body
    Right (bodyTy, CLet name defCore bodyCore)

  Lam param ty body -> do
    (bodyTy, bodyCore) <- elab (Map.insert param ty env) body
    Right (TFun ty bodyTy, CLam ty param bodyCore)

  App f arg -> do
    (fTy, fCore) <- elab env f
    case fTy of
      TFun paramTy retTy -> do
        argCore <- elab env arg >>= expect paramTy
        Right (retTy, CApp fCore argCore)
      _ -> Left $ "Expected function type, got " <> show fTy

  Neg a -> do
    aCore <- elab env a >>= expect TInt
    Right (TInt, CNeg aCore)

  Add a b -> elabBinOp env CAdd a b
  Sub a b -> elabBinOp env CSub a b
  Mul a b -> elabBinOp env CMul a b
  Div a b -> elabBinOp env CDiv a b

elabBinOp :: TEnv -> (Core -> Core -> Core) -> Expr -> Expr -> Either String (Type, Core)
elabBinOp env op a b = do
  aCore <- elab env a >>= expect TInt
  bCore <- elab env b >>= expect TInt
  Right (TInt, op aCore bCore)

elaborate :: Expr -> Either String (Type, Core)
elaborate = elab Map.empty
