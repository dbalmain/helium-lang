module Helium.Check (elaborate) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helium.Core (Core (..))
import Helium.Syntax (Expr (..), Span, Type (..), TypeError (TypeError), spanOf)

type TEnv = Map String Type

expect :: Span -> Type -> (Type, Core) -> Either TypeError Core
expect s expected (actual, core)
  | expected == actual = Right core
  | otherwise = Left $ TypeError s $ "Expected " <> show expected <> ", got " <> show actual

elab :: TEnv -> Expr -> Either TypeError (Type, Core)
elab env expr = case expr of
  Lit _ x -> Right (TInt, CLit x)
  Var s name -> case Map.lookup name env of
    Just ty -> Right (ty, CVar name)
    Nothing -> Left $ TypeError s $ "Unbound variable: " <> name

  Let _ name def body -> do
    (defTy, defCore) <- elab env def
    (bodyTy, bodyCore) <- elab (Map.insert name defTy env) body
    Right (bodyTy, CLet name defCore bodyCore)

  Lam _ param ty body -> do
    (bodyTy, bodyCore) <- elab (Map.insert param ty env) body
    Right (TFun ty bodyTy, CLam ty param bodyCore)

  App s f arg -> do
    (fTy, fCore) <- elab env f
    case fTy of
      TFun paramTy retTy -> do
        argCore <- elab env arg >>= expect (spanOf arg) paramTy
        Right (retTy, CApp fCore argCore)
      _ -> Left $ TypeError s $ "Expected function type, got " <> show fTy

  Neg _ a -> do
    aCore <- elab env a >>= expect (spanOf a) TInt
    Right (TInt, CNeg aCore)

  Add _ a b -> elabBinOp env CAdd a b
  Sub _ a b -> elabBinOp env CSub a b
  Mul _ a b -> elabBinOp env CMul a b
  Div _ a b -> elabBinOp env CDiv a b

elabBinOp :: TEnv -> (Core -> Core -> Core) -> Expr -> Expr -> Either TypeError (Type, Core)
elabBinOp env op a b = do
  aCore <- elab env a >>= expect (spanOf a) TInt
  bCore <- elab env b >>= expect (spanOf b) TInt
  Right (TInt, op aCore bCore)

elaborate :: Expr -> Either TypeError (Type, Core)
elaborate = elab Map.empty
