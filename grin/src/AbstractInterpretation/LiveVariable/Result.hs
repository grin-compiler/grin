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

data LVAResult
  = LVAResult
  { _memory   :: Vector Liveness
  , _register :: Map Name Liveness
  , _function :: Map Name (Liveness, Vector Liveness)
  }
  deriving (Eq, Show)

emptyLVAResult :: LVAResult
emptyLVAResult = LVAResult mempty mempty mempty

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
  { _memory   = V.map convertHeapNodeSet _memory
  , _register = Map.map convertReg _absRegisterMap
  , _function = Map.map convertFunctionRegs _absFunctionArgMap
  }
  where
    isLive :: Set LivenessId -> Bool
    isLive s = Set.member (-1) s || Set.member (-2) s

    convertReg :: Reg -> Liveness
    convertReg (Reg i) = convertValue $ _register V.! (fromIntegral i)

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

    convertValue :: R.Value -> Liveness
    convertValue (R.Value vals ns)
      | Map.null . R._nodeTagMap $ ns = BasicVal (isLive vals)
      | otherwise = convertNodeSet ns

    convertFunctionRegs :: (Reg, [Reg]) -> (Liveness, Vector Liveness)
    convertFunctionRegs (Reg retReg, argRegs) = (convertValue $ _register V.! (fromIntegral retReg), V.fromList [convertValue $ _register V.! (fromIntegral argReg) | Reg argReg <- argRegs])
