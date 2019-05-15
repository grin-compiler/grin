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
  let extIdMap = Map.fromList . map swap . Map.toList . Map.map extID $ _sExternalMap
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
      leftExp >>= \case
      {-
        Z -> case lpat of
          Unit -> pure ()
          Var name -> do
            r <- newReg
            emit IR.Set {dstReg = r, constant = IR.CSimpleType unitType}
            addReg name r
          _ -> error $ "pattern mismatch at HPT bind codegen, expected Unit got " ++ show lpat
      -}
        R r -> case lpat of
          -- Unit  -> pure ()
          -- Lit{} -> pure ()
          Var name -> addReg name r
          {-
          ConstTagNode tag args -> do
            irTag <- getTag tag
            bindInstructions <- forM (zip [0..] args) $ \(idx, arg) -> case arg of
              Var name -> do
                argReg <- newReg
                addReg name argReg
                pure [IR.Project {srcSelector = IR.NodeItem irTag idx, srcReg = r, dstReg = argReg}]
              Lit {} -> pure []
              _ -> error $ "illegal node pattern component " ++ show arg
            -- QUESTION: In HPTProgram the instructions are in reverse order, here they are in regular order, isn't this inconsistent?
            -- A: each cpat argument has zero or one instruction
            --    the order of cpat binding evaluation does not matter because they does not depend on each other
            emit IR.If
              { condition     = IR.NodeTypeExists irTag
              , srcReg        = r
              , instructions  = concat bindInstructions
              }
          -}
          _ -> error $ "unsupported lpat " ++ show lpat
      rightExp

    ECaseF val alts_ -> do
      caseResultReg <- newReg
      altRegs <- sequence alts_
      forM altRegs $ \(R altReg) ->
        emit IR.Move { srcReg = altReg, dstReg = caseResultReg }
      pure $ R caseResultReg

    AltF cpat exp -> pure $ A cpat exp

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
