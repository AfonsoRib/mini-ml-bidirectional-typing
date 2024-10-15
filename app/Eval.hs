module Eval where

import Expr
import Parser
import Context 

-- Evaluate the parsed expression to an integer or double
eval :: Context -> Expr -> Value
eval _ (BoolLit n) = BoolVal n
eval ctx (Var x) = case lookupVar x ctx of
  Just x -> x
  Nothing -> error "Variable not found"
eval ctx (If cond t f) =
  case eval ctx cond of
    BoolVal True -> eval ctx t
    BoolVal False -> eval ctx f
    _ -> error "Type mismatch"
-- Todo: implement Abs, App, and Ann
  
