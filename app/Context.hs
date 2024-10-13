module Context where

import Expr
import Parser

import qualified Data.Map as Map

data Context = Context { vars :: Map.Map String Value } deriving (Show)

-- Create an empty context
emptyContext :: Context
emptyContext = Context { vars = Map.empty }

lookupVar :: String -> Context -> Maybe Value
lookupVar x ctx = Map.lookup x (vars ctx)

addVar :: String -> Value -> Context -> Context
addVar x e ctx = Context { vars = Map.insert x e (vars ctx) }

data TypeContext = TypeContext { typeVars :: Map.Map String Type } deriving (Show)

emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext { typeVars = Map.empty }

lookupTypeVar :: String -> TypeContext -> Maybe Type
lookupTypeVar x ctx = Map.lookup x (typeVars ctx)

addTypeVar :: String -> Type -> TypeContext -> TypeContext
addTypeVar x t ctx = TypeContext { typeVars = Map.insert x t (typeVars ctx) }
