module Typechecker where

import Expr
import Parser
import Context

-- Evaluate the parsed expression to an integer or double
typechecker :: TypeContext -> Expr -> Type
typechecker _ (Lit n) = IntType
typechecker _ (FloatLit n) = DoubleType
typechecker ctx (Add x y) =
  case (typechecker ctx x, typechecker ctx y) of
    (IntType, IntType) -> IntType
    (DoubleType, DoubleType) -> DoubleType
    (IntType, DoubleType) -> DoubleType
    (DoubleType, IntType) -> DoubleType
typechecker ctx (Var x) =
  case lookupTypeVar x ctx of
    Just x -> x
    Nothing -> error "Variable not found"
typechecker ctx (Let x t e1 e2) =
  let t1 = typechecker ctx e1
      ctx' = addTypeVar x t1 ctx
  in
    if t /= t1 then error "Type mismatch" else
    typechecker ctx' e2
