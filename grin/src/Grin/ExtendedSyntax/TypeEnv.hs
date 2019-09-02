{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Grin.ExtendedSyntax.TypeEnv
  ( module Grin.ExtendedSyntax.TypeEnv
  , module Grin.ExtendedSyntax.TypeEnvDefs
  , module Grin.ExtendedSyntax.SyntaxDefs
  ) where

import Text.Printf
import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Set as Set (fromList, toList)
import qualified Data.Vector as Vector (fromList, toList, map)
import Data.Bifunctor (bimap)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Functor.Infix ((<$$>))
import Control.Applicative (liftA2)
import Control.Monad (join)

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.TypeEnvDefs
import Grin.ExtendedSyntax.SyntaxDefs


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

_T_String :: Traversal' SimpleType ()
_T_String f T_String = const T_String <$> f ()
_T_String _ rest     = pure rest

_T_Float :: Traversal' SimpleType ()
_T_Float f T_Float = const T_Float <$> f ()
_T_Float _ rest    = pure rest

_T_Unit :: Traversal' SimpleType ()
_T_Unit f T_Unit = const T_Unit <$> f ()
_T_Unit _ rest   = pure rest

_ReturnType :: Traversal' (Type, Vector Type) Type
_ReturnType = _1

_T_OnlyOneTag :: Traversal' NodeSet NodeSet
_T_OnlyOneTag f nodeSet
  | (Map.size nodeSet == 1) = f nodeSet
  | otherwise = pure nodeSet

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
  Nothing -> error $ printf "variable %s is missing from type environment" (unNM name)
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
  LString{} -> T_String
  LChar{}   -> T_Char

-- Type of literal like values
typeOfVal :: Val -> Type
typeOfVal = \case
  ConstTagNode  tag [] ->
    T_NodeSet
      $ Map.singleton tag
      $ mempty

  Unit    -> T_SimpleType T_Unit
  Lit lit -> typeOfLit lit

  bad -> error (show bad)

typeOfValTE :: TypeEnv -> Val -> Type
typeOfValTE typeEnv val = fromMaybe (error $ show val) $ mTypeOfValTE typeEnv val

mTypeOfValTE :: TypeEnv -> Val -> Maybe Type
mTypeOfValTE typeEnv@TypeEnv{..} = \case
  Undefined t -> Just t

  ConstTagNode tag args -> do
    tys <- mapM (`Map.lookup` _variable) args
    let sTys = map _simpleType $ tys
    pure . T_NodeSet . Map.singleton tag . Vector.fromList $ sTys

  Unit      -> Just $ T_SimpleType T_Unit
  Lit lit   -> Just $ typeOfLit lit
  Var name  -> typeEnv ^. variable . at name

  bad -> Nothing

-- | Sort locations, remove duplication from set like things.
normalizeTypeEnv :: TypeEnv -> TypeEnv
normalizeTypeEnv (TypeEnv locations variables functions) =
  TypeEnv
    (Vector.map normalizeNodeSet locations)
    (Map.map normalizeType variables)
    (Map.map (bimap normalizeType (Vector.map normalizeType)) functions)

normalizeSimpleType :: SimpleType -> SimpleType
normalizeSimpleType = \case
  T_Location ls -> T_Location $ Set.toList $ Set.fromList ls
  rest          -> rest

normalizeNodeSet :: NodeSet -> NodeSet
normalizeNodeSet = Map.map (Vector.map normalizeSimpleType)

normalizeType :: Type -> Type
normalizeType = \case
  T_SimpleType st -> T_SimpleType $ normalizeSimpleType st
  T_NodeSet    ns -> T_NodeSet $ normalizeNodeSet ns
  rest            -> rest

-- | Compare types, return Nothing if types are incomparable: Dead or UnspecifiedLocation
sameType :: Type -> Type -> Maybe Bool
sameType (T_SimpleType T_Dead) _                = Nothing
sameType _ (T_SimpleType T_Dead)                = Nothing
sameType (T_SimpleType T_UnspecifiedLocation) _ = Nothing
sameType _ (T_SimpleType T_UnspecifiedLocation) = Nothing
sameType t1 t2 = Just $ t1 == t2

ptrLocations :: TypeEnv -> Name -> [Loc]
ptrLocations te p = case variableType te p of
  T_SimpleType (T_Location locs) -> locs
  ty -> error $ "Variable " ++ show (PP p) ++ " should be a pointer, but instead it has type: " ++ show (PP ty)
