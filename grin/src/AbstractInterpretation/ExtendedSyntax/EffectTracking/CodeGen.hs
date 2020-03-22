{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, TemplateHaskell, OverloadedStrings #-}
module AbstractInterpretation.ExtendedSyntax.EffectTracking.CodeGen where

import Control.Monad.State

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.Functor.Foldable as Foldable
import Data.Tuple
import Data.Maybe

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.TypeEnv
import qualified AbstractInterpretation.ExtendedSyntax.IR as IR
import AbstractInterpretation.ExtendedSyntax.IR (Instruction(..), AbstractProgram(..), AbstractMapping(..))
import AbstractInterpretation.ExtendedSyntax.EffectTracking.CodeGenBase

codeGen :: Program -> (AbstractProgram, ETMapping)
codeGen prg@(Program exts defs) = evalState (codeGenM prg >> mkAbstractProgramM) emptyCGState
codeGen _ = error "Program expected"

mkAbstractProgramM :: CG (AbstractProgram, ETMapping)
mkAbstractProgramM = do
  CGState{..} <- get
  let prg = AbstractProgram
        { _absMemoryCounter   = 0
        , _absRegisterCounter = _sRegisterCounter
        , _absInstructions    = _sInstructions
        }
  let splitExt = (,) <$> extID <*> extExt
      extIdMap = Map.fromList . map splitExt . Map.elems $ _sExternalMap
  let mpg = ETMapping
        { _etRegisterMap     = _sRegisterMap
        , _etFunctionRetMap  = _sFunctionRetMap
        , _etExternalMap     = extIdMap
        }
  pure (prg, mpg)

codeGenM :: Exp -> CG Result
codeGenM = cata folder where
  folder :: ExpF (CG Result) -> CG Result
  folder = \case
    ProgramF exts defs -> mapM_ addExternal exts >> sequence_ defs >> pure Z

    DefF name args body -> do
      instructions <- state $ \s@CGState{..} -> (_sInstructions, s {_sInstructions = []})
      funResultReg <- getOrAddFunRetReg name
      body >>= \case
        -- Z   -> emit IR.Set  {dstReg = funResultReg, constant = IR.CSimpleType unitType}
        R r -> emit IR.Move {srcReg = r, dstReg = funResultReg}
      modify' $ \s@CGState{..} -> s {_sInstructions = reverse _sInstructions ++ instructions}
      pure Z

    EBindF leftExp bPat rightExp -> do
      lhs <- leftExp
      rhs <- rightExp
      let R lhsReg = lhs
      let R rhsReg = rhs

      case bPat of
        VarPat var     -> addReg var lhsReg
        AsPat  _ _ var -> addReg var lhsReg

      emit IR.Move { srcReg = lhsReg, dstReg = rhsReg }
      pure $ R rhsReg

    ECaseF val alts_ -> do
      caseResultReg <- newReg
      altRegs <- sequence alts_
      forM altRegs $ \(A _ altNameReg altM) -> do
        altResult <- altM
        case altResult of
          R altResultReg -> do
            -- NOTE: Propagate the effect info back to both the case result register and the alternative's register as well
            emit IR.Move { srcReg = altResultReg, dstReg = caseResultReg }
          -- TODO: maybe put altName into "A" as well?
          _ -> error $ "Effect tracking: a case alternative did not return a register. Scrutinee was: " ++ show (PP val)
      pure $ R caseResultReg

    -- NOTE: Currently, the names of the alternatives are ignored by the analysis.
    AltF cpat n exp -> do
      altNameReg <- newReg
      addReg n altNameReg
      pure $ A cpat altNameReg exp

    SAppF name args -> getExternal name >>= \case
      Just ext  -> do
        appReg <- newReg
        extID  <- fromJust <$> getExternalID name
        when (eEffectful ext) $
          emit IR.Set  { dstReg = appReg, constant = IR.CSimpleType extID }
        pure $ R appReg

        -----------

      Nothing   -> do
        appReg <- newReg
        funResultReg <- getOrAddFunRetReg name
        emit IR.Move { srcReg = funResultReg, dstReg = appReg }
        pure $ R appReg

    SReturnF{} -> R <$> newReg

    SStoreF{} -> R <$> newReg

    SFetchF{}-> R <$> newReg

    SUpdateF{} -> R <$> newReg

    SBlockF exp -> exp
