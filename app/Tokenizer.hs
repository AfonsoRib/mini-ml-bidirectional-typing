module Tokenizer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (liftA2)


data Token = TTrue | TFalse | TIf | TThen |TElse deriving (Show, Eq)

trueParser :: Parser Token
trueParser = string "true" >> return TTrue

falseParser :: Parser Token
falseParser = string "false" >> return TFalse

ifParser :: Parser Token
ifParser = string "if" >> return TIf

thenParser :: Parser Token
thenParser = string "then" >> return TThen

  
tokenParser :: Parser Token
tokenParser = try (string "true" >> return TTrue) <|>
              try (string "false" >> return TFalse) <|>
              try (string "if" >> return TIf) <|>
              try (string "then" >> return TThen) <|>
              (string "else" >> return TElse)



tokensParser :: Parser [Token]
tokensParser = many (tokenParser <* spaces)

-- Parse
parseStr :: String -> Either ParseError [Token]
parseStr = parse tokensParser ""




