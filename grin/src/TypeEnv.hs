{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module TypeEnv where

import Text.Printf
import Data.Int
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Vector as Vector (fromList)
import Data.Monoid

import Lens.Micro.Platform

import Grin

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  | T_Location {_locations :: [Int]}
  deriving (Eq, Ord, Show)

type NodeSet = Map Tag (Vector SimpleType)

data Type
  = T_SimpleType  {_simpleType  :: SimpleType}
  | T_NodeSet     {_nodeSet     :: NodeSet}

  -- dependent type constructions to describe deconstructed nodes
  | T_Tag         {_tagDomain   :: NodeSet}   -- tag with it's corresponding node set domain
  | T_Item        {_tagVariable :: Name       -- tag variable name that holds the node tag on which the item type depends
                  ,_itemIndex   :: Int        -- item index in the node
                  }
  deriving (Eq, Ord, Show)

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


data TypeEnv
  = TypeEnv
  { _location :: Vector NodeSet
  , _variable :: Map Name Type
  , _function :: Map Name (Type, Vector Type)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''TypeEnv, ''Type, ''SimpleType]

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv mempty mempty mempty

newVar :: Name -> Type -> Endo TypeEnv
newVar n t = Endo (variable %~ (Map.insert n t))

create :: Endo TypeEnv -> TypeEnv
create (Endo c) = c emptyTypeEnv

extend :: TypeEnv -> Endo TypeEnv -> TypeEnv
extend t (Endo c) = c t

variableType :: TypeEnv -> Name -> Type
variableType TypeEnv{..} name = case Map.lookup name _variable of
  Nothing -> error $ printf "variable %s is missing from type environment" name
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
