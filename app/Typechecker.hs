module Typechecker where

import Expr
import Parser

-- Evaluate the parsed expression to an integer or double
typechecker :: Expr -> Type
typechecker (Lit n) = IntType
typechecker (FloatLit n) = DoubleType
typechecker (Add x y)   =
  case (typechecker x, typechecker y) of
    (IntType, IntType) -> IntType
    (DoubleType, DoubleType) -> DoubleType
    (IntType, DoubleType) -> DoubleType
    (DoubleType, IntType) -> DoubleType
