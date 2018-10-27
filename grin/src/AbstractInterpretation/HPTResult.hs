{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies #-}
module AbstractInterpretation.HPTResult where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Lens.Micro.Platform
import Lens.Micro.Internal

import Grin.Grin (Name, Tag)
import qualified Grin.TypeEnv as TypeEnv

type Loc = Int

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  | T_Location Loc
  | T_String
  | T_Char
  deriving (Eq, Ord, Show)

newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set SimpleType))} deriving (Eq, Ord, Show)

data TypeSet
  = TypeSet
  { _simpleType :: Set SimpleType
  , _nodeSet    :: NodeSet
  }
  deriving (Eq, Ord, Show)

instance Semigroup  NodeSet where (<>)    = unionNodeSet
instance Monoid     NodeSet where mempty  = NodeSet mempty

instance Semigroup  TypeSet where (<>)    = unionTypeSet
instance Monoid     TypeSet where mempty  = TypeSet mempty mempty

unionNodeSet :: NodeSet -> NodeSet -> NodeSet
unionNodeSet (NodeSet x) (NodeSet y) = NodeSet $ Map.unionWith unionNodeData x y where
  unionNodeData a b
    | V.length a == V.length b = V.zipWith Set.union a b
    | otherwise = error $ "node arity mismatch " ++ show (V.length a) ++ " =/= " ++ show (V.length b)

unionTypeSet :: TypeSet -> TypeSet -> TypeSet
unionTypeSet a b = TypeSet
  { _simpleType = Set.union (_simpleType a) (_simpleType b)
  , _nodeSet    = unionNodeSet (_nodeSet a) (_nodeSet b)
  }

data HPTResult
  = HPTResult
  { _memory   :: Vector NodeSet
  , _register :: Map Name TypeSet
  , _function :: Map Name (TypeSet, Vector TypeSet)
  , _sharing  :: Set Loc
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''NodeSet, ''TypeSet, ''HPTResult]

toSimpleType :: Int32 -> SimpleType
toSimpleType ty | ty < 0 = case ty of
  -1 -> T_Unit
  -2 -> T_Int64
  -3 -> T_Word64
  -4 -> T_Float
  -5 -> T_Bool
  -6 -> T_String
  -7 -> T_Char
  _ -> error $ "unknown type code " ++ show ty
toSimpleType l = T_Location $ fromIntegral l

type instance Index   (Vector a) = Int
type instance IxValue (Vector a) = a

instance At (Vector a) where
  at k = lens (V.!? k) (\v -> maybe v (\a -> v V.// [(k, a)]))

_T_Location :: Traversal' SimpleType Int
_T_Location f (T_Location l) = T_Location <$> f l
_T_Location _ rest           = pure rest
