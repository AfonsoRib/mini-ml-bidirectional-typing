module Typechecker where

import Expr
import Parser
import Context

-- Evaluate the parsed expression to an integer or double
inferType :: TypeContext -> Expr -> Maybe Type
inferType _ (BoolLit _) = Just BoolType
inferType ctx (Var x) =
  case lookupTypeVar x ctx of
    Just x -> Just x
    Nothing -> Nothing   
inferType ctx (Ann e ty) = checkType ctx e ty
inferType ctx (App t1 t2) = do
  FunType ty1 ty2 <- inferType ctx t1
  checkType ctx t2 ty1
  return ty2
inferType _ _ = Nothing


checkType :: TypeContext -> Expr -> Type -> Maybe Type
checkType ctx (If t1 t2 t3) ty = 
  case (checkType ctx t1 BoolType,
        checkType ctx t2 ty,
        checkType ctx t3 ty) of
    (Just BoolType, Just ty1, Just ty2) -> Just ty
    _ -> Nothing
checkType ctx (Abs (Var x) body) (FunType ty1 ty2) =
  case checkType (addTypeVar x ty1 ctx) body ty2 of
    Just ty2' -> if ty2 == ty2' then Just (FunType ty1 ty2) else Nothing
    Nothing -> Nothing
checkType ctx (Abs _ _) _ = Nothing
checkType ctx (App t1 t2) ty =
  case
    (inferType ctx t1,
     checkType ctx t2 ty) of
    (Just (FunType ty1 ty2), Just ty1') -> if ty1 == ty1' then Just ty2 else Nothing
    _ -> Nothing
checkType ctx t ty = 
  case inferType ctx t of
    Just ty' -> if ty == ty' then Just ty else Nothing
    Nothing -> Nothing
