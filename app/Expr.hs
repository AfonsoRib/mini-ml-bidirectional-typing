module Expr where
-- Define the data type for arithmetic expressions
data Expr = Var String
          | BoolLit Bool
          | If Expr Expr Expr
          | Abs Expr Expr
          | App Expr Expr
          | Ann Expr Type
          deriving (Show)

data Type = BoolType
          | FunType Type Type
          deriving (Show, Eq)

data Value = BoolVal Bool
           | Closure String Expr Type
           deriving (Show)


