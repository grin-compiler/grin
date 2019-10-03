{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, DeriveFunctor, ViewPatterns #-}
module AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap

import Lens.Micro.Platform
import Lens.Micro.Extra

import Grin.ExtendedSyntax.Grin (Name, Tag)
import AbstractInterpretation.ExtendedSyntax.IR hiding (Tag, SimpleType)
import AbstractInterpretation.ExtendedSyntax.Reduce (ComputerState(..))
import qualified Grin.ExtendedSyntax.TypeEnv as TypeEnv
import qualified AbstractInterpretation.ExtendedSyntax.IR as IR
import qualified AbstractInterpretation.ExtendedSyntax.Reduce as R
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGenBase (HPTMapping)

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
  | T_UnspecifiedLocation
  {- NOTE: The local value can be used for any analysis-specific computation,
           but cannot be propagated to the type checking phase.
  -}
  | Local HPTLocal
  deriving (Eq, Ord, Show)

data HPTLocal = UndefinedProducer
  deriving (Eq, Ord, Show)

undefinedProducer :: IR.SimpleType
undefinedProducer = -1723

fromHPTLocal :: HPTLocal -> IR.SimpleType
fromHPTLocal UndefinedProducer = undefinedProducer

toHPTLocal :: IR.SimpleType -> HPTLocal
toHPTLocal t
  | t == undefinedProducer = UndefinedProducer
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

emptyHPTResult :: HPTResult
emptyHPTResult = HPTResult mempty mempty mempty

concat <$> mapM makeLenses [''NodeSet, ''TypeSet, ''HPTResult]

-- Negative integers less than (-8) can represent any analysis-specific value.
toSimpleType :: IR.SimpleType -> SimpleType
toSimpleType = \case
  -1 -> T_Unit
  -2 -> T_Int64
  -3 -> T_Word64
  -4 -> T_Float
  -5 -> T_Bool
  -6 -> T_String
  -7 -> T_Char
  -8 -> T_UnspecifiedLocation
  ty | ty < 0    -> Local $ toHPTLocal ty
     | otherwise -> T_Location $ fromIntegral ty

_T_Location :: Traversal' SimpleType Int
_T_Location f (T_Location l) = T_Location <$> f l
_T_Location _ rest           = pure rest

toHPTResult :: HPTMapping -> R.ComputerState -> HPTResult
toHPTResult a@AbstractMapping{..} c@R.ComputerState{..} = HPTResult
  { _memory   = V.map (convertNodeSet s) _memory
  , _register = Map.map (convertReg s) _absRegisterMap
  , _function = Map.map (convertFunctionRegs s) _absFunctionArgMap
  }
  where
    s = (a,c)

convertReg :: (HPTMapping, ComputerState) -> Reg -> TypeSet
convertReg s@(AbstractMapping{..}, R.ComputerState{..}) (Reg i) = convertValue s $ _register V.! (fromIntegral i)

convertNodeSet :: (HPTMapping, ComputerState) -> R.NodeSet -> NodeSet
convertNodeSet s@(AbstractMapping{..}, R.ComputerState{..}) (R.NodeSet a) = NodeSet $ Map.fromList [(_absTagMap Bimap.!> k, V.map convertSimpleType v) | (k,v) <- Map.toList a]

convertSimpleType :: Set IR.SimpleType -> Set SimpleType
convertSimpleType = Set.map toSimpleType

convertValue :: (HPTMapping, ComputerState) -> R.Value -> TypeSet
convertValue s@(AbstractMapping{..}, R.ComputerState{..}) (R.Value ty ns) = TypeSet (convertSimpleType ty) (convertNodeSet s ns)

convertFunctionRegs :: (HPTMapping, ComputerState) -> (Reg, [Reg]) -> (TypeSet, Vector TypeSet)
convertFunctionRegs s@(AbstractMapping{..}, R.ComputerState{..}) (Reg retReg, argRegs) = (convertValue s $ _register V.! (fromIntegral retReg), V.fromList [convertValue s $ _register V.! (fromIntegral argReg) | Reg argReg <- argRegs])
