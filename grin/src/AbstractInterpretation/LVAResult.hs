{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, DeriveFunctor, ViewPatterns #-}
module AbstractInterpretation.LVAResult where

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
import AbstractInterpretation.LiveVariable (LVAProgram(..))
import AbstractInterpretation.IR as IR hiding (Tag, SimpleType, Liveness)
import qualified AbstractInterpretation.IR as IR (Liveness)
import qualified AbstractInterpretation.Reduce as R

newtype Node = Node { _node :: Vector Bool }
  deriving (Eq, Ord, Show)

data Liveness = BasicVal Bool
              | NodeSet (Map Tag Node)
  deriving (Eq, Ord, Show)

data LVAResult
  = LVAResult
  { _memory   :: Vector Liveness
  , _register :: Map Name Liveness
  , _function :: Map Name (Liveness, Vector Liveness)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''Node, ''Liveness, ''LVAResult]


toLVAResult :: LVAProgram -> R.Computer -> LVAResult
toLVAResult (getDataFlowInfo -> AbstractProgram{..}) R.Computer{..} = LVAResult
  { _memory   = V.map convertHeapNodeSet _memory
  , _register = Map.map convertReg absRegisterMap
  , _function = Map.map convertFunctionRegs absFunctionArgMap
  }
  where
    isLive :: Set IR.Liveness -> Bool
    isLive = Set.member (-1)

    convertReg :: Reg -> Liveness
    convertReg (Reg i) = convertValue $ _register V.! (fromIntegral i)

    -- we can encounter empty node sets on the heap
    convertHeapNodeSet :: R.NodeSet -> Liveness
    convertHeapNodeSet rns@(R.NodeSet ns)
      | Map.null ns = BasicVal False
      | otherwise   = convertNodeSet rns

    convertNodeSet :: R.NodeSet -> Liveness
    convertNodeSet (R.NodeSet ns) = NodeSet $ Map.mapKeys fromIR irTaggedMap
      where irTaggedMap = Map.map (Node . V.map isLive) ns
            fromIR irTag = absTagMap Bimap.!> irTag

    convertValue :: R.Value -> Liveness
    convertValue (R.Value vals ns)
      | Map.null . R._nodeTagMap $ ns = BasicVal (isLive vals)
      | otherwise = convertNodeSet ns

    convertFunctionRegs :: (Reg, [Reg]) -> (Liveness, Vector Liveness)
    convertFunctionRegs (Reg retReg, argRegs) = (convertValue $ _register V.! (fromIntegral retReg), V.fromList [convertValue $ _register V.! (fromIntegral argReg) | Reg argReg <- argRegs])
