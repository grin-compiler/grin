{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module AbstractInterpretation.Reduce
  ( evalHPT
  , toHPTResult
  ) where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap

import Control.Monad.State.Strict
import Lens.Micro.Platform

import AbstractInterpretation.IR
import AbstractInterpretation.CodeGen
import qualified AbstractInterpretation.HPTResultNew as R

newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set Int32))} deriving (Eq, Show)

data Value
  = Value
  { _simpleType :: Set Int32
  , _nodeSet    :: NodeSet
  }
  deriving (Eq, Show)

data Computer
  = Computer
  { _memory    :: Vector NodeSet
  , _register  :: Vector Value
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''NodeSet, ''Value, ''Computer]

type HPT = State Computer

instance Monoid NodeSet where
  mempty  = NodeSet mempty
  mappend = unionNodeSet

instance Monoid Value where
  mempty  = Value mempty mempty
  mappend = unionValue

unionNodeSet :: NodeSet -> NodeSet -> NodeSet
unionNodeSet (NodeSet x) (NodeSet y) = NodeSet $ Map.unionWith unionNodeData x y where
  unionNodeData a b
    | V.length a == V.length b = V.zipWith Set.union a b
    | otherwise = error $ "node arity mismatch " ++ show (V.length a) ++ " =/= " ++ show (V.length b)

unionValue :: Value -> Value -> Value
unionValue a b = Value
  { _simpleType = Set.union (_simpleType a) (_simpleType b)
  , _nodeSet    = unionNodeSet (_nodeSet a) (_nodeSet b)
  }

regIndex :: Reg -> Int
regIndex (Reg i) = fromIntegral i

memIndex :: Mem -> Int
memIndex (Mem i) = fromIntegral i

evalInstruction :: Instruction -> HPT ()
evalInstruction = \case
  If {..} -> do
    satisfy <- case condition of
      NodeTypeExists tag -> do
        tagMap <- use $ register.ix (regIndex srcReg).nodeSet.nodeTagMap
        pure $ Map.member tag tagMap
      SimpleTypeExists ty -> do
        typeSet <- use $ register.ix (regIndex srcReg).simpleType
        pure $ Set.member ty typeSet
      HasMoreThan tags -> do
        tagMap <- use $ register.ix (regIndex srcReg).nodeSet.nodeTagMap
        typeSet <- use $ register.ix (regIndex srcReg).simpleType
        pure $ not (Set.null typeSet) || Set.isProperSubsetOf tags (Map.keysSet tagMap)
    when satisfy $ mapM_ evalInstruction instructions

  Project {..} -> do
    let NodeItem tag itemIndex = srcSelector
    value <- use $ register.ix (regIndex srcReg).nodeSet.nodeTagMap.at tag.non mempty.ix itemIndex
    register.ix (regIndex dstReg).simpleType %= (mappend value)

  Extend {..} -> do
    value <- use $ register.ix (regIndex srcReg).simpleType
    let NodeItem tag itemIndex = dstSelector
    register.ix (regIndex dstReg).nodeSet.nodeTagMap.at tag.non mempty.ix itemIndex %= (mappend value)

  Move {..} -> do
    value <- use $ register.ix (regIndex srcReg)
    register.ix (regIndex dstReg) %= (mappend value)

  Fetch {..} -> do
    addressSet <- use $ register.ix (regIndex addressReg).simpleType
    forM_ addressSet $ \address -> when (address >= 0) $ do
      value <- use $ memory.ix (fromIntegral address)
      register.ix (regIndex dstReg).nodeSet %= (mappend value)

  Store {..} -> do
    value <- use $ register.ix (regIndex srcReg).nodeSet
    memory.ix (memIndex address) %= (mappend value)

  Update {..} -> do
    value <- use $ register.ix (regIndex srcReg).nodeSet
    addressSet <- use $ register.ix (regIndex addressReg).simpleType
    forM_ addressSet $ \address -> when (address >= 0) $ do
      memory.ix (fromIntegral address) %= (mappend value)

  Set {..} -> case constant of
    CSimpleType ty        -> register.ix (regIndex dstReg).simpleType %= (mappend $ Set.singleton ty)
    CHeapLocation (Mem l) -> register.ix (regIndex dstReg).simpleType %= (mappend $ Set.singleton $ fromIntegral l)
    CNodeType tag arity   -> register.ix (regIndex dstReg).nodeSet %=
                                (mappend $ NodeSet . Map.singleton tag $ V.replicate arity mempty)
    CNodeItem tag idx val -> register.ix (regIndex dstReg).nodeSet.
                                nodeTagMap.at tag.non mempty.ix idx %= (mappend $ Set.singleton val)

evalHPT :: HPTProgram -> Computer
evalHPT HPTProgram{..} = run emptyComputer where
  emptyComputer = Computer
    { _memory   = V.replicate (fromIntegral hptMemoryCounter) mempty
    , _register = V.replicate (fromIntegral hptRegisterCounter) mempty
    }
  run computer = if computer == nextComputer then computer else run nextComputer
    where nextComputer = execState (mapM_ evalInstruction hptInstructions) computer

toHPTResult :: HPTProgram -> Computer -> R.HPTResult
toHPTResult HPTProgram{..} Computer{..} = R.HPTResult
  { R._memory   = V.map convertNodeSet _memory
  , R._register = Map.map convertReg hptRegisterMap
  , R._function = Map.map convertFunctionRegs hptFunctionArgMap

  }
  where
    convertReg :: Reg -> R.TypeSet
    convertReg (Reg i) = convertValue $ _register V.! (fromIntegral i)

    convertNodeSet :: NodeSet -> R.NodeSet
    convertNodeSet (NodeSet a) = R.NodeSet $ Map.fromList [(hptTagMap Bimap.!> k, V.map convertSimpleType v) | (k,v) <- Map.toList a]

    convertSimpleType :: Set Int32 -> Set R.SimpleType
    convertSimpleType s = Set.map R.toSimpleType s

    convertValue :: Value -> R.TypeSet
    convertValue (Value ty ns) = R.TypeSet (convertSimpleType ty) (convertNodeSet ns)

    convertFunctionRegs :: (Reg, [Reg]) -> (R.TypeSet, Vector R.TypeSet)
    convertFunctionRegs (Reg retReg, argRegs) = (convertValue $ _register V.! (fromIntegral retReg), V.fromList [convertValue $ _register V.! (fromIntegral argReg) | Reg argReg <- argRegs])
