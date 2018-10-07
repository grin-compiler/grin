{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Grin.TypeEnv 
  ( module Grin.TypeEnv 
  , module Grin.TypeEnvDefs 
  ) where

import Text.Printf
import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Vector as Vector (fromList)
import Data.Monoid

import Lens.Micro.Platform

import Grin.Grin
import Grin.TypeEnvDefs

dead_t :: Type
dead_t = T_SimpleType T_Dead

unit_t :: Type
unit_t = T_SimpleType T_Unit

int64_t :: Type
int64_t = T_SimpleType T_Int64

bool_t :: Type
bool_t = T_SimpleType T_Bool

float_t :: Type
float_t = T_SimpleType T_Float

location_t :: [Int] -> Type
location_t = T_SimpleType . T_Location

fun_t :: Name -> [Type] -> Type -> Map Name (Type, Vector Type)
fun_t name params ret = Map.singleton name (ret, Vector.fromList params)

cnode_t :: Name -> [SimpleType] -> NodeSet
cnode_t name params = Map.singleton (Tag C name) (Vector.fromList params)

-- * Prism

_T_NodeSet :: Traversal' Type NodeSet
_T_NodeSet f (T_NodeSet ns) = T_NodeSet <$> f ns
_T_NodeSet _ rest           = pure rest

_T_SimpleType :: Traversal' Type SimpleType
_T_SimpleType f (T_SimpleType s) = T_SimpleType <$> f s
_T_SimpleType _ rest             = pure rest

_T_Location :: Traversal' SimpleType [Int]
_T_Location f (T_Location ls) = T_Location <$> f ls
_T_Location _ rest            = pure rest

_T_Unit :: Traversal' SimpleType ()
_T_Unit f T_Unit = const T_Unit <$> f ()
_T_Unit _ rest   = pure rest

_ReturnType :: Traversal' (Type, Vector Type) Type
_ReturnType = _1

_T_OnlyOneTag :: Traversal' NodeSet NodeSet
_T_OnlyOneTag f nodeSet
  | (Map.size nodeSet == 1) = f nodeSet
  | otherwise = pure nodeSet


emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv mempty mempty mempty

newVar :: Name -> Type -> Endo TypeEnv
newVar n t = Endo (variable %~ (Map.insert n t))

newFun :: Name -> Type -> [Type] -> Endo TypeEnv
newFun n t a = Endo (function %~ (Map.insert n (t, Vector.fromList a)))

create :: Endo TypeEnv -> TypeEnv
create (Endo c) = c emptyTypeEnv

extend :: TypeEnv -> Endo TypeEnv -> TypeEnv
extend t (Endo c) = c t

variableType :: TypeEnv -> Name -> Type
variableType TypeEnv{..} name = case Map.lookup name _variable of
  Nothing -> error $ printf "variable %s is missing from type environment" name
  Just t -> t

functionType :: TypeEnv -> Name -> (Type, Vector Type)
functionType TypeEnv{..} name = case Map.lookup name _function of
  Nothing -> error $ printf "function %s is missing from type environment" name
  Just t -> t

typeOfLit :: Lit -> Type
typeOfLit = T_SimpleType . typeOfLitST

typeOfLitST :: Lit -> SimpleType
typeOfLitST lit = case lit of
  LInt64{}  -> T_Int64
  LWord64{} -> T_Word64
  LFloat{}  -> T_Float
  LBool{}   -> T_Bool

-- Type of literal like values
typeOfVal :: Val -> Type
typeOfVal = \case
  ConstTagNode  tag simpleVals ->
    T_NodeSet
      $ Map.singleton tag
      $ Vector.fromList
      $ map ((\(T_SimpleType t) -> t) . typeOfVal) simpleVals

  Unit    -> T_SimpleType T_Unit
  Lit lit -> typeOfLit lit

  bad -> error (show bad)

typeOfValTE :: TypeEnv -> Val -> Type
typeOfValTE typeEnv = \case
  ConstTagNode  tag simpleVals ->
    T_NodeSet
      $ Map.singleton tag
      $ Vector.fromList
      $ map ((\(T_SimpleType t) -> t) . typeOfValTE typeEnv) simpleVals

  Unit      -> T_SimpleType T_Unit
  Lit lit   -> typeOfLit lit
  Var name  -> variableType typeEnv name

  bad -> error (show bad)

-- * Effects

data Effect
  = Effectful Name
  | Update { updateLocs :: [Int] }
  deriving (Eq, Show, Ord)

type EffectMap = Map Name (Set Effect)
