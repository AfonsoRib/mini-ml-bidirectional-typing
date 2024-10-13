module Expr where
-- Define the data type for arithmetic expressions
data Expr = Add Expr Expr
          | Lit Integer
          | FloatLit Double
          | Var String
          | Let String Type Expr Expr -- Para já o campo de type não é
                                      -- necessário mas pode ser útil
                                      -- no futuro para o
                                      -- bidirectional type checking
          deriving (Show)

data Type = IntType
          | DoubleType
          deriving (Show, Eq)

data Value = IntVal Integer
           | DoubleVal Double
           deriving (Show)


