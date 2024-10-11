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

-- Parser for double literals with lexeme
doubleLiteral :: Parser Expr
doubleLiteral = lexeme $ FloatLit <$> float
  
-- Parser for terms
term :: Parser Expr
term = try doubleLiteral <|> try integerLiteral <|> try parens

-- Parser for addition
addition :: Parser Expr
addition = do
  x <- term
  lexeme $ char '+'
  y <- expr
  return $ Add x y

-- Parser for parens
parens :: Parser Expr
parens = do
  lexeme $ char '('
  x <- expr
  lexeme $ char ')'
  return x

-- Parser for expressions
expr :: Parser Expr
expr =  try addition <|> try term 

-- parse expre
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces *> expr <* spaces) ""
