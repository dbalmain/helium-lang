module Helium.Core (Core (..), lint) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helium.Syntax (Type (..))

data Core
  = CLit Int
  | CVar String
  | CLet String Core Core
  | CLam Type String Core
  | CApp Core Core
  | CNeg Core
  | CAdd Core Core
  | CSub Core Core
  | CMul Core Core
  | CDiv Core Core
  deriving (Show)

type TEnv = Map String Type

expect :: Type -> Type -> Either String ()
expect expected actual
  | expected == actual = Right ()
  | otherwise = Left $ "lint: Expected " <> show expected <> ", got " <> show actual

lintBinOp :: TEnv -> Core -> Core -> Either String Type
lintBinOp env a b = do
  lint env a >>= expect TInt
  lint env b >>= expect TInt
  Right TInt

lint :: TEnv -> Core -> Either String Type
lint env core = case core of
  CLit _ -> Right TInt
  CVar name -> maybe (Left $ "lint: Unbound variable: " <> name) Right (Map.lookup name env)
  CLet name def body -> do
    defTy <- lint env def
    lint (Map.insert name defTy env) body
  CLam paramTy param body -> do
    bodyTy <- lint (Map.insert param paramTy env) body
    Right $ TFun paramTy bodyTy
  CApp f arg -> do
    fTy <- lint env f
    case fTy of
      TFun paramTy retTy -> do
        argTy <- lint env arg
        expect paramTy argTy
        Right retTy
      _ -> Left $ "lint: Expected function type, got " <> show fTy
  CNeg a -> do
    lint env a >>= expect TInt
    Right TInt
  CAdd a b -> lintBinOp env a b
  CSub a b -> lintBinOp env a b
  CMul a b -> lintBinOp env a b
  CDiv a b -> lintBinOp env a b
