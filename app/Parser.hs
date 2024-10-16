{-# LANGUAGE RecursiveDo #-}
module Parser where

import Tokenizer
import Control.Applicative
import Text.Earley
import Expr

tokenGrammar :: Grammar r (Prod r [Token] Token Expr)
tokenGrammar = mdo
  trueExpr <- rule $ token TTrue *> pure (BoolLit True)
  falseExpr <- rule $ token TFalse *> pure (BoolLit False)
  expr <- rule $ trueExpr <|> falseExpr <|> ifExpr
  ifExpr  <- rule $ If
    <$> (token TIf *> expr)    -- Parse the condition after matching 'if'
    <*> (token TThen *> expr)  -- Parse the true branch after 'then'
    <*> (token TElse *> expr)  -- Parse the false branch after 'else'
  return expr

parseExpr :: [Token] -> Maybe Expr
parseExpr tokens = case fullParses (parser tokenGrammar) tokens of
  ([e], _) -> Just e
  _        -> Nothing
