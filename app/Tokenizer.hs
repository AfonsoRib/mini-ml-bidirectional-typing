module Tokenizer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (liftA2)


data Token = TTrue
           | TFalse
           | TIf
           | TThen
           | TElse
           | TColon
           | TBool
           | TVar String
           | TArrow
           | TLambda
           | TDot
           | TLPar
           | TRPar
           deriving (Show, Eq)

trueParser :: Parser Token
trueParser = string "true" >> return TTrue

falseParser :: Parser Token
falseParser = string "false" >> return TFalse

ifParser :: Parser Token
ifParser = string "if" >> return TIf

thenParser :: Parser Token
thenParser = string "then" >> return TThen

variableParser :: Parser Token
variableParser = many1 letter >>= \x -> return (TVar x)

  
tokenParser :: Parser Token
tokenParser = try (string "true" >> return TTrue) <|>
              try (string "false" >> return TFalse) <|>
              try (string "if" >> return TIf) <|>
              try (string "then" >> return TThen) <|>
              try (string "else" >> return TElse) <|>
              try (string ":" >> return TColon) <|>
              try (string "Bool" >> return TBool) <|>
              try (string "->" >> return TArrow) <|>
              try (string "\\" >> return TLambda) <|>
              try (string "." >> return TDot) <|>
              try (string "(" >> return TLPar) <|>
              try (string ")" >> return TRPar) <|>
              try variableParser
              



tokensParser :: Parser [Token]
tokensParser = many (tokenParser <* spaces)

tokenize :: String -> Either ParseError [Token]
tokenize = parse tokensParser ""




