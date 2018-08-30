{-# LANGUAGE LambdaCase #-}
module AbstractInterpretation.Sharing
  ( Mode(..)
  , sharingCodeGen
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
-}

data Mode = UpdatesInEval | InlinedEval

calcNonLinearVariables :: Mode -> Exp -> Set.Set Name
calcNonLinearVariables mode exp = Set.fromList $ Map.keys $ Map.filter (>1) $ cata collect exp
  where
    union = Map.unionsWith (+)
    collect = \case
      ECaseF val alts -> union (seen val : alts)
      SStoreF val -> seen val
      SFetchIF var _ -> seen (Var var)
      SUpdateF var val ->
        case mode of
          UpdatesInEval -> mempty
          InlinedEval   -> seen val
      SReturnF val -> seen val
      SAppF _ ps -> union $ fmap seen ps
      rest -> Data.Foldable.foldr (Map.unionWith (+)) mempty rest

    seen = \case
      Var v -> Map.singleton v 1
      ConstTagNode _ ps -> union $ fmap seen ps
      VarTagNode v ps -> union $ fmap seen (Var v : ps)
      _ -> Map.empty

sharingCodeGen :: Mode -> Exp -> CG ()
sharingCodeGen m e = do
  forM_ nonLinearVars $ \name -> do
    -- For all non-linear variables set the locations as shared.
    nonLinearVarReg <- getReg name
    nonLinearVarLocReg <- newReg
    emit $ IR.Project Locations nonLinearVarReg nonLinearVarLocReg
    emit $ IR.SetShared nonLinearVarLocReg

  sharedLocReg <- newReg
  pointsToLocReg <- newReg
  emit $ IR.GetShared sharedLocReg
  emit $ IR.Project IR.NodeLocations sharedLocReg pointsToLocReg
  emit $ IR.SetShared pointsToLocReg
  where
    nonLinearVars = calcNonLinearVariables m e
