module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Expr
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

-- lexeme
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- Parser for variables
variable :: Parser Expr
variable = Var <$> lexeme (many1 letter)

-- Parser for annotations
annotation :: Parser Expr
annotation = do
  x <- expression
  lexeme $ char ':'
  t <- typeExpr
  return $ Ann x t

-- Parser for Booleans
boolean :: Parser Expr
boolean = (lexeme $ string "True" >> return (BoolLit True)) <|> (lexeme $ string "False" >> return (BoolLit False))

-- Parser for terms
expression :: Parser Expr
expression = try boolean <|> try variable <|> try parens

-- Parser for basic types
basicType :: Parser Type
basicType = (lexeme (string "Bool") >> return BoolType)

-- Parser for function types
functionType :: Parser Type
functionType = do
  argType <- basicType <|> parensType
  lexeme $ string "->"
  returnType <- typeExpr
  return $ FunType argType returnType

-- Parser for types (basic or function)
typeExpr :: Parser Type
typeExpr = try functionType <|> basicType <|> parensType

-- Parser for types within parentheses
parensType :: Parser Type
parensType = do
  lexeme $ char '('
  t <- typeExpr
  lexeme $ char ')'
  return t

  
-- Parser for if statement
ifStatement :: Parser Expr
ifStatement = do
  lexeme $ string "if"
  cond <- statement
  lexeme $ string "then"
  t <- statement
  lexeme $ string "else"
  f <- statement
  return $ If cond t f

-- Parser for functions
function :: Parser Expr
function = do
  lexeme $ string "Lambda"
  var <- variable
  lexeme $ string "."
  body <- statement
  return $ Abs var body

-- -- Parser for function application
-- application :: Parser Expr
-- application = do
--   f <- statement
--   x <- statement
--   return $ App f x

-- Parser for parens
parens :: Parser Expr
parens = do
  lexeme $ char '('
  x <- statement
  lexeme $ char ')'
  return x

-- Parser for expressions
statement :: Parser Expr
statement = try annotation <|>
            try function <|>
            try ifStatement <|>
            try expression

-- parse expr
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces *> statement <* spaces) ""
