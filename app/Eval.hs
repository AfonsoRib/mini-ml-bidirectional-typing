module Eval where

import Expr
import Parser
import Context 

-- Evaluate the parsed expression to an integer or double
eval :: Context -> Expr -> Value
eval _ (Lit n) = IntVal n
eval _ (FloatLit n) = DoubleVal n
eval ctx (Add x y) =
  case (eval ctx x, eval ctx y) of
    (IntVal n, IntVal m) -> IntVal (n + m)
    (DoubleVal n, DoubleVal m) -> DoubleVal (n + m)
    (IntVal n, DoubleVal m) -> DoubleVal (fromIntegral n + m)
    (DoubleVal n, IntVal m) -> DoubleVal (n + fromIntegral m)
eval ctx (Var x) = case lookupVar x ctx of
  Just x -> x
  Nothing -> error "Variable not found"
eval ctx (Let x _ e1 e2) =
  let v1 = eval ctx e1
      ctx' = addVar x v1 ctx
  in
    eval ctx' e2
