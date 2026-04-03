module Helium.Eval (eval, Val (..)) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helium.Core (Core (..))

type Env = Map String Val

data Val
  = VInt Int
  | VClosure String Core Env
  deriving (Show)

binOp :: Env -> (Int -> Int -> Int) -> Core -> Core -> Either String Val
binOp env op coreA coreB = do
  valA <- eval env coreA
  valB <- eval env coreB
  case (valA, valB) of
    (VInt a, VInt b) -> Right . VInt $ op a b
    _ -> error "impossible: type checker guarantees integer operands"

eval :: Env -> Core -> Either String Val
eval env core = case core of
  CLit x -> Right $ VInt x
  CVar name -> Right $ env Map.! name
  CLet name coreDef coreBody -> do
    val <- eval env coreDef
    eval (Map.insert name val env) coreBody
  CLam _ty param body -> Right $ VClosure param body env
  CApp coreF coreArg -> do
    valF <- eval env coreF
    case valF of
      VClosure param body closureEnv -> do
        valArg <- eval env coreArg
        eval (Map.insert param valArg closureEnv) body
      _ -> error "impossible: type checker guarantees function application"
  CNeg coreA -> do
    valA <- eval env coreA
    case valA of
      VInt a -> Right . VInt $ negate a
      _ -> error "impossible: type checker guarantees integer operand"
  CAdd coreA coreB -> binOp env (+) coreA coreB
  CSub coreA coreB -> binOp env (-) coreA coreB
  CMul coreA coreB -> binOp env (*) coreA coreB
  CDiv coreA coreB -> do
    valA <- eval env coreA
    valB <- eval env coreB
    case (valA, valB) of
      (VInt a, VInt b) ->
        if b == 0 then Left "Division by 0" else Right . VInt $ a `div` b
      _ -> error "impossible: type checker guarantees integer operands"
