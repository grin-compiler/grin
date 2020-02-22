{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Grin.TypeEnv
  ( module Grin.TypeEnv
  , module Grin.TypeEnvDefs
  , module Grin.SyntaxDefs
  ) where

import Text.Printf
import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Bifunctor (bimap)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Functor.Infix ((<$$>))
import Control.Applicative (liftA2)
import Control.Monad (join)

import Lens.Micro.Platform

import Grin.Grin
import Grin.Pretty
import Grin.TypeEnvDefs
import Grin.SyntaxDefs


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
  ConstTagNode  tag simpleVals ->
    T_NodeSet
      $ Map.singleton tag
      $ Vector.fromList
      $ map ((\(T_SimpleType t) -> t) . typeOfVal) simpleVals

  Unit    -> T_SimpleType T_Unit
  Lit lit -> typeOfLit lit

  bad -> error (show bad)

typeOfValTE :: TypeEnv -> Val -> Type
typeOfValTE typeEnv val = fromMaybe (error $ show val) $ mTypeOfValTE typeEnv val

mTypeOfValTE :: TypeEnv -> Val -> Maybe Type
mTypeOfValTE typeEnv = \case
  Undefined t -> Just t

  ConstTagNode tag simpleVals ->
    fmap (T_NodeSet . Map.singleton tag . Vector.fromList)
      $ sequenceA $ map (fmap (\(T_SimpleType t) -> t) . mTypeOfValTE typeEnv) simpleVals

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

subType :: Type -> Type -> Maybe Bool
subType (T_SimpleType T_Dead) _ = Nothing
subType _ (T_SimpleType T_Dead) = Nothing
subType (T_SimpleType T_UnspecifiedLocation) _ = Nothing
subType _ (T_SimpleType T_UnspecifiedLocation) = Nothing
subType (T_SimpleType st1) (T_SimpleType st2) = Just $ simpleSubType st1 st2
subType (T_NodeSet t1) (T_NodeSet t2) = do -- t1 <: t2
  let ks1     = Map.keysSet t1
  let ks2     = Map.keysSet t2
  let subset  = ks1 `Set.isSubsetOf` ks2
  pure $ subset && and  [ Vector.length v1 == Vector.length v2
                          && (Vector.all id (Vector.zipWith simpleSubType v1 v2))
                        | (t,v1)  <- Map.toList t1
                        , Just v2 <- pure $ Map.lookup t t2
                        ]
subType _ _ = Nothing

simpleSubType :: SimpleType -> SimpleType -> Bool
simpleSubType T_UnspecifiedLocation (T_Location l) = True
simpleSubType (T_Location ls1) (T_Location ls2) =
  (Set.fromList ls1) `Set.isSubsetOf` (Set.fromList ls2)
simpleSubType t1 t2 = t1 == t2

ptrLocations :: TypeEnv -> Name -> [Loc]
ptrLocations te p = case variableType te p of
  T_SimpleType (T_Location locs) -> locs
  ty -> error $ "Variable " ++ show (PP p) ++ " should be a pointer, but instead it has type: " ++ show (PP ty)
