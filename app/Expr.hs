module Expr where
-- Define the data type for arithmetic expressions
data Expr = Add Expr Expr
          | Lit Integer
          | FloatLit Double
          deriving (Show)

data Type = IntType
          | DoubleType
          deriving (Show)

data Value = IntVal Integer
           | DoubleVal Double
           deriving (Show)


