module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Expr
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)


lexer = Tok.makeTokenParser emptyDef
float = Tok.float lexer
integer = Tok.integer lexer

-- lexeme
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- Parser for integer literals
integerLiteral :: Parser Expr
integerLiteral = lexeme $ Lit <$> integer

-- Parser for double literals 
doubleLiteral :: Parser Expr
doubleLiteral = lexeme $ FloatLit <$> float

-- Parser for variables
variable :: Parser Expr
variable = Var <$> lexeme (many1 letter)
-- Parser for terms
expression :: Parser Expr
expression = try doubleLiteral <|> try integerLiteral <|> try parens <|> variable

-- Parser for addition
addition :: Parser Expr
addition = do
  x <- expression
  lexeme $ char '+'
  y <- statement
  return $ Add x y

-- Parser for let statement
letStatement :: Parser Expr
letStatement = do
  lexeme $ string "let"
  var <- lexeme $ many1 letter
  lexeme $ char ':'
  type' <- lexeme $ string "Int" <|> string "Double"
  t <- case type' of
    "Int" -> return IntType
    "Double" -> return DoubleType
    _ -> fail "Invalid type"
  lexeme $ char '='
  val <- statement
  lexeme $ string "in"
  body <- statement
  return $ Let var t val body

-- Parser for parens
parens :: Parser Expr
parens = do
  lexeme $ char '('
  x <- statement
  lexeme $ char ')'
  return x

-- Parser for expressions
statement :: Parser Expr
statement =  try letStatement <|>
             try addition <|>
             try expression 

-- parse expre
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces *> statement <* spaces) ""
