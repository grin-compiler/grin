{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module AbstractInterpretation.LVAResult
( module AbstractInterpretation.LVAResult
, module AbstractInterpretation.LVAResultTypes
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap

import AbstractInterpretation.LiveVariable (LVAProgram(..))
import AbstractInterpretation.LVAResultTypes
import AbstractInterpretation.IR as IR hiding (Liveness)
import qualified AbstractInterpretation.IR as IR (Liveness)
import qualified AbstractInterpretation.Reduce as R

toLVAResult :: LVAProgram -> R.Computer -> LVAResult
toLVAResult (getDataFlowInfo -> AbstractProgram{..}) R.Computer{..} = LVAResult
  { _memory   = V.map convertHeapNodeSet _memory
  , _register = Map.map convertReg _absRegisterMap
  , _function = Map.map convertFunctionRegs _absFunctionArgMap
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

    convertFields :: V.Vector (Set IR.Liveness) -> Node
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
