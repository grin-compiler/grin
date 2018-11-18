{-# LANGUAGE LambdaCase #-}
module AbstractInterpretation.Sharing
  ( sharingCodeGen
  ) where

import Grin.Syntax
import Data.Functor.Foldable
import AbstractInterpretation.CodeGen
import Data.Foldable
import AbstractInterpretation.IR as IR

import qualified Data.Map as Map
import qualified Data.Set as Set

{-
[x] Calc non-linear variables, optionally ignoring updates.
[x] Assumption: all the non-linear variables have registers already.
[x] For all non-linear variables set the locations as shared.
[x] For all the shared locations, look up the pointed locations and set to shared them.

[x] Create a register to maintain sharing information
[x] Add Sharing register as parameter to HPT
[x] Add Sharing register as parameter to Sharing
[x] Remove _shared field from Reduce
[-] Add 'In' to the Condition
[x] Remove GetShared from IR
[x] Remove SetShared from IR
[x] CodeGenMain: depend on optimisation phase, before InLineAfter inline
    [x] CodeGenMain: add an extra parameter
    [x] Pipeline: Set a variable if the codegen is after or before
[-] Add mode as parameter for the eval'' and typecheck
[x] Remove Mode for calcNonLinearVars
-}

-- | Calc non linear variables, ignores variables that are used in update locations
-- This is an important difference, if a variable would become non-linear due to
-- being subject to an update, that would make the sharing analysis incorect.
-- One possible improvement is to count the updates in a different set and make a variable
-- linear if it subject to an update more than once. But that could not happen, thus the only
-- introdcution of new updates comes from inlining the eval.
calcNonLinearNonUpdateLocVariables :: Exp -> Set.Set Name
calcNonLinearNonUpdateLocVariables exp = Set.fromList $ Map.keys $ Map.filter (>1) $ cata collect exp
  where
    union = Map.unionsWith (+)
    collect = \case
      ECaseF val alts -> union (seen val : alts)
      SStoreF val -> seen val
      SFetchIF var _ -> seen (Var var)
      SUpdateF var val -> seen val
      SReturnF val -> seen val
      SAppF _ ps -> union $ fmap seen ps
      rest -> Data.Foldable.foldr (Map.unionWith (+)) mempty rest

    seen = \case
      Var v -> Map.singleton v 1
      ConstTagNode _ ps -> union $ fmap seen ps
      VarTagNode v ps -> union $ fmap seen (Var v : ps)
      _ -> Map.empty

sharingCodeGen :: IR.Reg -> Exp -> CG ()
sharingCodeGen s e = do
  forM_ nonLinearVars $ \name -> do
    -- For all non-linear variables set the locations as shared.
    nonLinearVarReg <- getReg name
    nonLinearVarLocReg <- newReg
    emit $ IR.Project Locations nonLinearVarReg nonLinearVarLocReg
    emit $ IR.Move nonLinearVarLocReg s

  pointsToLocReg <- newReg
  pointsToNodeReg <- newReg
  emit $ IR.Fetch s pointsToNodeReg
  emit $ IR.Project IR.NodeLocations pointsToNodeReg pointsToLocReg
  emit $ IR.Move pointsToLocReg s
  where
    nonLinearVars = calcNonLinearNonUpdateLocVariables e
