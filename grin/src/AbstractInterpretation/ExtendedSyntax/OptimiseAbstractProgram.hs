{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module AbstractInterpretation.ExtendedSyntax.OptimiseAbstractProgram where

import AbstractInterpretation.ExtendedSyntax.IR

import Data.List (sortBy)
import Data.Maybe (maybeToList)
import Data.Graph hiding (edges)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Debug.Trace

{-
Optimisation needs further investigation.
-}

optimiseAbstractProgram :: AbstractProgram -> AbstractProgram
optimiseAbstractProgram p@AbstractProgram{..} = p { _absInstructions = sortProgram _absInstructions }
  where
    sortedNodes = absSortedNodes p
    compareIns i0 i1
      | Just d <- dst i0
      , s <- src i1
      , Just j0 <- Map.lookup d sortedNodes
      , Just j1 <- Map.lookup s sortedNodes
      = compare j0 j1

      | Nothing <- dst i0
      = LT
    compareIns (If{}) (Store{}) = GT
    compareIns (Store{}) (If{}) = LT
    compareIns _ _ = EQ

    sortProgram is =
        sortBy compareIns
      $ map (\case {If{..} -> If condition srcReg (sortProgram instructions); i -> i})
      $ is

instructionCount :: AbstractProgram -> Int
instructionCount = sum . map go . _absInstructions
  where
    go :: Instruction -> Int
    go = \case
      If{..} -> sum $ map go instructions
      _      -> 1

data Node = R Reg | C Constant
  deriving (Eq, Show, Ord)

src :: Instruction -> Node
src = \case
  If{..}      -> R srcReg
  Project{..} -> R srcReg
  Extend{..}  -> R srcReg
  Move{..}    -> R srcReg
  Fetch{..}   -> R addressReg
  Store{..}   -> R srcReg
  Update{..}  -> R srcReg
  Set{..}     -> C constant

dst :: Instruction -> Maybe Node
dst = \case
  If{..}      -> Nothing
  Project{..} -> Just $ R dstReg
  Extend{..}  -> Just $ R dstReg
  Move{..}    -> Just $ R dstReg
  Fetch{..}   -> Just $ R dstReg
  Store{..}   -> Nothing
  Update{..}  -> Just $ R addressReg
  Set{..}     -> Just $ R dstReg

edges :: Instruction -> [(Node, Node)]
edges = \case
  If{..} -> concatMap edges instructions
  ins    -> maybeToList $ fmap ((,) (src ins)) $ dst ins

absSortedNodes :: AbstractProgram -> Map.Map Node Int
absSortedNodes AbstractProgram{..}
  = Map.fromList
  . (`zip` [0..])
--  $ traceShowId
  . flattenSCCs
  . stronglyConnComp
  . edgeSet
  . concatMap edges
  $ _absInstructions
  where
    edgeSet :: [(Node, Node)] -> [(Node, Node, [Node])]
    edgeSet nodeList
      = map (\(f,ts) -> (f,f,Set.toList ts))
      $ Map.toList
      $ Map.map Set.fromList
      $ Map.unionsWith (++)
      $ map (\(f,t) -> Map.singleton f [t])
      $ nodeList
