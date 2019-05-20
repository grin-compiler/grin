{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, TemplateHaskell, OverloadedStrings #-}
module AbstractInterpretation.EffectTracking.CodeGen where

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

import Grin.Grin
import Grin.TypeEnv
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (Instruction(..), AbstractProgram(..), AbstractMapping(..))
import AbstractInterpretation.EffectTracking.CodeGenBase

-- cannot return Z anywhere, any computation can contain side effects

returnNewReg :: CG Result
returnNewReg = do
  r <- newReg
  pure $ R r

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

    EBindF leftExp lpat rightExp -> do
      lhs <- leftExp
      rhs <- rightExp
      let R lhsReg = lhs
      let R rhsReg = rhs

      case lpat of
        Unit     -> pure ()
        Var name -> addReg name lhsReg
        _ -> error $ "Effect tracking: unsupported lpat " ++ show lpat

      emit IR.Move { srcReg = lhsReg, dstReg = rhsReg }
      pure $ R rhsReg

    ECaseF val alts_ -> do
      caseResultReg <- newReg
      altRegs <- sequence alts_
      forM altRegs $ \(R altReg) ->
        emit IR.Move { srcReg = altReg, dstReg = caseResultReg }
      pure $ R caseResultReg

    AltF _ exp -> exp

    SAppF name args -> getExternal name >>= \case
      Just ext  -> do
        appReg <- newReg
        extID  <- fromJust <$> getExternalID name
        if eEffectful ext then
          emit IR.Set  { dstReg = appReg, constant = IR.CSimpleType extID }
        else
          pure ()
        pure $ R appReg

        -----------

      Nothing   -> do
        appReg <- newReg
        funResultReg <- getOrAddFunRetReg name
        emit IR.Move { srcReg = funResultReg, dstReg = appReg }
        pure $ R appReg

    SReturnF{} -> returnNewReg

    SStoreF{} -> returnNewReg

    SFetchIF{}-> returnNewReg

    SUpdateF{} -> returnNewReg

    SBlockF exp -> exp
