{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module AbstractInterpretation.ExtendedSyntax.Sharing.CodeGen where

import Control.Monad.State

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector, (!))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import qualified Data.Set.Extra as Set

import Data.Maybe
import Data.Foldable
import Data.Functor.Foldable

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Syntax
import Grin.ExtendedSyntax.TypeEnvDefs
import AbstractInterpretation.ExtendedSyntax.Util (converge)
import AbstractInterpretation.ExtendedSyntax.IR (Instruction(..), Reg, AbstractProgram)
import qualified AbstractInterpretation.ExtendedSyntax.IR as IR

import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGenBase
import qualified AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGen as HPT

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

data SharingMapping = SharingMapping
  { _shRegName  :: Reg
  , _hptMapping :: HPTMapping
  } deriving (Show)

concat <$> mapM makeLenses [''SharingMapping]

-- | Calc non linear variables, ignores variables that are used in update locations
-- This is an important difference, if a variable would become non-linear due to
-- being subject to an update, that would make the sharing analysis incorrect.
-- One possible improvement is to count the updates in a different set and make a variable
-- linear if it is subject to an update more than once. But that could not happen, thus the only
-- introdcution of new updates comes from inlining eval.
calcNonLinearNonUpdateLocVariables :: Exp -> Set Name
calcNonLinearNonUpdateLocVariables exp = Set.fromList $ Map.keys $ Map.filter (>1) $ cata collect exp
  where
    union = Map.unionsWith (+)

    collect :: ExpF (Map Name Int) -> Map Name Int
    collect = \case
      ECaseF scrut alts -> union (seen scrut : alts)
      SStoreF var -> seen var
      SFetchF var -> seen var
      SUpdateF p var -> seen var
      SReturnF val -> case val of
        Var v               -> seen v
        ConstTagNode _ args -> union $ map seen args
        _                   -> mempty
      SAppF _ ps -> union $ fmap seen ps
      rest -> Data.Foldable.foldr (Map.unionWith (+)) mempty rest

    seen :: Name -> Map Name Int
    seen v = Map.singleton v 1

calcSharedLocationsPure :: TypeEnv -> Exp -> Set Loc
calcSharedLocationsPure TypeEnv{..} exp = converge (==) (foldMap fetchLoc) rootLocs where
  rootLocs :: Set Loc
  rootLocs = Set.fromList
           . concatMap reachableLocs
           . toList
           . calcNonLinearNonUpdateLocVariables
           $ exp

  fetchLoc :: Loc -> Set Loc
  fetchLoc i = Set.fromList $ i : locsFromNodeSet (_location ! i)

  -- collects all the locations that might be reached directly from a given variable
  reachableLocs :: Name -> [Loc]
  reachableLocs var = locsFromTy $ fromJust $ Map.lookup var _variable

  locsFromSTy :: SimpleType -> [Loc]
  locsFromSTy sty = (sty ^. locations)

  locsFromNodeSet :: NodeSet -> [Loc]
  locsFromNodeSet = concatMap locsFromSTy
                  . concatMap toList
                  . Map.elems

  locsFromTy :: Type -> [Loc]
  locsFromTy (T_SimpleType sty) = locsFromSTy sty
  locsFromTy (T_NodeSet ns)     = locsFromNodeSet ns


sharingCodeGen :: Reg -> Exp -> CG ()
sharingCodeGen shReg e = do
  forM_ nonLinearVars $ \name -> do
    -- For all non-linear variables set the locations as shared.
    nonLinearVarReg <- getReg name
    -- this will copy node field info as well, but we will only use "simpleType" info
    emit $ copyStructureWithPtrInfo nonLinearVarReg shReg

  -- Collect all potential pointers in shared node fields
  -- into the simple type part of the register.
  -- This will collect non-location simple types as well, but we will ignore them.
  emit IR.Project
    { srcReg      = shReg
    , srcSelector = IR.AllFields
    , dstReg      = shReg
    }
  -- Fetch all the values from the shared locations
  -- into the sime type part of the register.
  -- This will collect non-location simple types as well, but we will ignore them.
  emit IR.Fetch
    { addressReg = shReg
    , dstReg     = shReg
    }
  where
    nonLinearVars = calcNonLinearNonUpdateLocVariables e

codeGenM :: Exp -> CG (AbstractProgram, SharingMapping)
codeGenM e = do
  HPT.codeGenM e
  shReg <- newReg
  sharingCodeGen shReg e
  (prg, hptMapping) <- HPT.mkAbstractProgramM
  let mapping = SharingMapping
        { _shRegName  = shReg
        , _hptMapping = hptMapping
        }
  pure (prg, mapping)

codeGen :: Program -> (AbstractProgram, SharingMapping)
codeGen prg@(Program{}) = evalState (codeGenM prg) emptyCGState
codeGen _ = error "Program expected"
