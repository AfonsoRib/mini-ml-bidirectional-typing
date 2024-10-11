module Eval where

import Expr
import Parser

-- Evaluate the parsed expression to an integer or double
eval :: Expr  -> Value
eval (Lit n) = IntVal n
eval (FloatLit n) = DoubleVal n
eval (Add x y)   =
  case (eval x, eval y) of
    (IntVal n, IntVal m) -> IntVal (n + m)
    (DoubleVal n, DoubleVal m) -> DoubleVal (n + m)
    (IntVal n, DoubleVal m) -> DoubleVal (fromIntegral n + m)
    (DoubleVal n, IntVal m) -> DoubleVal (n + fromIntegral m)
