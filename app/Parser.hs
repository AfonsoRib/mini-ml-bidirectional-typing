{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Parser where

import Tokenizer
import Control.Applicative
import Text.Earley
import Data.Char
import Expr

tokenGrammar :: Grammar r (Prod r [Token] Token Expr)
tokenGrammar = mdo
  trueExpr <- rule $ token TTrue *> pure (BoolLit True)
  falseExpr <- rule $ token TFalse *> pure (BoolLit False)
  ifExpr  <- rule $ If
    <$> (token TIf *> expr)
    <*> (token TThen *> expr)
    <*> (token TElse *> expr)
  expr <- rule $ trueExpr <|> falseExpr <|> ifExpr <|> var
  var <- rule $ (\(TVar x) -> Var x) <$> satisfy isVar
  return expr
  where isVar (TVar _) = True
        isVar _ = False


parseExpr :: [Token] -> Maybe Expr
parseExpr tokens = case fullParses (parser tokenGrammar) tokens of
  ([e], _) -> Just e
  ([], _)  -> Nothing
  _        -> Nothing
