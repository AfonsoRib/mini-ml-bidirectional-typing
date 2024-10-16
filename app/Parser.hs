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
  --types
  boolType <- rule $ token TBool *> pure BoolType
  funType <- rule $ FunType <$> t <*> (token TArrow *> t)
  t <- rule $ boolType <|> funType
  --expr
  trueExpr <- rule $ token TTrue *> pure (BoolLit True)
  falseExpr <- rule $ token TFalse *> pure (BoolLit False)
  ifExpr  <- rule $ If
    <$> (token TIf *> statement)
    <*> (token TThen *> statement)
    <*> (token TElse *> statement)
  var <- rule $ (\(TVar x) -> Var x) <$> satisfy isVar
  ann <- rule $ Ann <$> statement <*> (token TColon *> t)
  abs <- rule $ Abs <$> (token TLambda *> var) <*> (token TDot *> statement)
  app <- rule $ App <$> statement <*> statement
  parens <- rule $ token TLPar *> statement <* token TRPar
  expr <- rule $ trueExpr <|> falseExpr <|> var 
  statement <- rule $ ifExpr <|> ann <|> abs <|> app <|> expr <|> parens
  return statement
  where isVar (TVar _) = True
        isVar _ = False

  
parseExpr :: [Token] -> Maybe Expr

parseExpr tokens = case fullParses (parser tokenGrammar) tokens of
  ([e], _) -> Just e
  _        -> Nothing
