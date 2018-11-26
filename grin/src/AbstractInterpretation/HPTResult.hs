{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, DeriveFunctor, ViewPatterns #-}
module AbstractInterpretation.HPTResult where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap

import Lens.Micro.Platform
import Lens.Micro.Internal

import Grin.Grin (Name, Tag)
import AbstractInterpretation.HeapPointsTo (HPTProgram(..))
import AbstractInterpretation.CreatedBy (undefinedProducer)
import AbstractInterpretation.IR as IR hiding (Tag, SimpleType)
import qualified Grin.TypeEnv as TypeEnv
import qualified AbstractInterpretation.IR as IR (SimpleType)
import qualified AbstractInterpretation.Reduce as R

type Loc = Int

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  | T_Location Loc
  | T_UnspecifiedLocation
  {- NOTE: The local value can be used for any analysis-specific computation,
           but cannot be propagated to the type checking phase.
  -}
  | Local HPTLocal
  deriving (Eq, Ord, Show)

data HPTLocal = UndefinedProducer
  deriving (Eq, Ord, Show)

fromHPTLocal :: HPTLocal -> IR.SimpleType
fromHPTLocal UndefinedProducer   = undefinedProducer

toHPTLocal :: IR.SimpleType -> HPTLocal
toHPTLocal t
  | t == undefinedProducer   = UndefinedProducer
toHPTLocal t = error $ "IR simple type " ++ show t ++ " cannot be convert to HPTLocal"

type    Node    = Vector (Set SimpleType)
newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag Node}
  deriving (Eq, Ord, Show)

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
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''NodeSet, ''TypeSet, ''HPTResult]

-- Negative integers less than (-6) can represent any analysis-specific value.
toSimpleType :: IR.SimpleType -> SimpleType
toSimpleType (-1) = T_Unit
toSimpleType (-2) = T_Int64
toSimpleType (-3) = T_Word64
toSimpleType (-4) = T_Float
toSimpleType (-5) = T_Bool
toSimpleType (-6) = T_UnspecifiedLocation
toSimpleType ty 
  | ty < 0    = Local $ toHPTLocal ty
  | otherwise = T_Location $ fromIntegral ty

type instance Index   (Vector a) = Int
type instance IxValue (Vector a) = a

instance At (Vector a) where
  at k = lens (V.!? k) (\v -> maybe v (\a -> v V.// [(k, a)]))

_T_Location :: Traversal' SimpleType Int
_T_Location f (T_Location l) = T_Location <$> f l
_T_Location _ rest           = pure rest

toHPTResult :: HPTProgram -> R.Computer -> HPTResult
toHPTResult (getDataFlowInfo -> AbstractProgram{..}) R.Computer{..} = HPTResult
  { _memory   = V.map convertNodeSet _memory
  , _register = Map.map convertReg _absRegisterMap
  , _function = Map.map convertFunctionRegs _absFunctionArgMap
  }
  where
    convertReg :: Reg -> TypeSet
    convertReg (Reg i) = convertValue $ _register V.! (fromIntegral i)

    convertNodeSet :: R.NodeSet -> NodeSet
    convertNodeSet (R.NodeSet a) = NodeSet $ Map.fromList [(_absTagMap Bimap.!> k, V.map convertSimpleType v) | (k,v) <- Map.toList a]

    convertSimpleType :: Set IR.SimpleType -> Set SimpleType
    convertSimpleType = Set.map toSimpleType

    convertValue :: R.Value -> TypeSet
    convertValue (R.Value ty ns) = TypeSet (convertSimpleType ty) (convertNodeSet ns)

    convertFunctionRegs :: (Reg, [Reg]) -> (TypeSet, Vector TypeSet)
    convertFunctionRegs (Reg retReg, argRegs) = (convertValue $ _register V.! (fromIntegral retReg), V.fromList [convertValue $ _register V.! (fromIntegral argReg) | Reg argReg <- argRegs])
