{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, ViewPatterns, RecordWildCards #-}
module AbstractInterpretation.LiveVariable.Result where

import Lens.Micro.Platform

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap

import Grin.Grin (Name, Tag)
import AbstractInterpretation.LiveVariable.CodeGen (LVAMapping)
import AbstractInterpretation.IR hiding (Tag)
import qualified AbstractInterpretation.Reduce as R

type LivenessId = Int32

data Node = Node
  { _tag    :: Bool
  , _fields :: Vector Bool
  }
  deriving (Eq, Ord, Show)

data Liveness
  = BasicVal Bool
  | NodeSet (Map Tag Node)
  deriving (Eq, Ord, Show)

newtype Effect = Effect { _hasEffect :: Bool }
  deriving (Eq, Ord, Show)

data LVAResult
  = LVAResult
  { _memory      :: Vector Liveness
  , _registerLv  :: Map Name Liveness
  , _functionLv  :: Map Name (Liveness, Vector Liveness)
  , _registerEff :: Map Name Effect
  , _functionEff :: Map Name Effect
  }
  deriving (Eq, Show)

emptyLVAResult :: LVAResult
emptyLVAResult = LVAResult mempty mempty mempty mempty mempty

concat <$> mapM makeLenses [''Node, ''Liveness, ''LVAResult]

isNodeLive :: Node -> Bool
isNodeLive = (||) <$> hasLiveTag <*> hasLiveField

hasLiveTag :: Node -> Bool
hasLiveTag (Node tagLv fieldsLv) = tagLv

hasLiveField :: Node -> Bool
hasLiveField (Node tagLv fieldsLv) = or fieldsLv

isLive :: Liveness -> Bool
isLive (BasicVal b) = b
isLive (NodeSet  m) = any isNodeLive m

hasLiveArgs :: (Liveness, Vector Liveness) -> Bool
hasLiveArgs (_, argsLv) = any isLive argsLv

-- | A function is only dead if its return value is dead
-- , and all of its parameters are dead as well. The case
-- when the return value is dead, but there is a live parameter
-- means that the function has some kind of side effect.
isFunDead :: (Liveness, Vector Liveness) -> Bool
isFunDead (retLv, argsLv) = not (isLive retLv || any isLive argsLv)

toLVAResult :: LVAMapping -> R.ComputerState -> LVAResult
toLVAResult AbstractMapping{..} R.ComputerState{..} = LVAResult
  { _memory      = V.map convertHeapNodeSet _memory
  , _registerLv  = Map.map convertRegLv _absRegisterMap
  , _functionLv  = Map.map convertFunctionRegs _absFunctionArgMap
  , _registerEff = Map.map convertRegEff _absRegisterMap
  , _functionEff = Map.map convertFunctionEffect _absFunctionArgMap
  }
  where
    isLive :: Set LivenessId -> Bool
    isLive = Set.member (-1)

    hasEffect :: Set LivenessId -> Bool
    hasEffect = Set.member (-2)

    convertReg :: (R.Value -> a) -> Reg -> a
    convertReg convertValue (Reg i) = convertValue $ _register V.! (fromIntegral i)

    convertRegLv :: Reg -> Liveness
    convertRegLv = convertReg convertValueLv

    convertRegEff :: Reg -> Effect
    convertRegEff = convertReg convertValueEff

    -- we can encounter empty node sets on the heap
    convertHeapNodeSet :: R.NodeSet -> Liveness
    convertHeapNodeSet rns@(R.NodeSet ns)
      | Map.null ns = BasicVal False
      | otherwise   = convertNodeSet rns

    convertFields :: V.Vector (Set LivenessId) -> Node
    convertFields vec = Node (isLive tagLv) (V.map isLive fieldsLv)
      where (tagLv, fieldsLv) = (,) <$> V.head <*> V.tail $ vec

    convertNodeSet :: R.NodeSet -> Liveness
    convertNodeSet (R.NodeSet ns) = NodeSet $ Map.mapKeys fromIR irTaggedMap
      where irTaggedMap = Map.map convertFields ns
            fromIR irTag = _absTagMap Bimap.!> irTag

    convertValueLv :: R.Value -> Liveness
    convertValueLv (R.Value vals ns)
      | Map.null . R._nodeTagMap $ ns = BasicVal (isLive vals)
      | otherwise = convertNodeSet ns

    convertFunctionRegs :: (Reg, [Reg]) -> (Liveness, Vector Liveness)
    convertFunctionRegs (Reg retReg, argRegs) = (convertValueLv $ _register V.! (fromIntegral retReg), V.fromList [convertValueLv $ _register V.! (fromIntegral argReg) | Reg argReg <- argRegs])

    convertValueEff :: R.Value -> Effect
    convertValueEff (R.Value vals _) = Effect (hasEffect vals)

    convertFunctionEffect :: (Reg, [Reg]) -> Effect
    convertFunctionEffect (retReg, _) = convertRegEff retReg
