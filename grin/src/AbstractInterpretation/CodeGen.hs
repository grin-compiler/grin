{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, RankNTypes, ViewPatterns #-}
module AbstractInterpretation.CodeGen where

import Control.Monad.Trans.Except
import Data.Word
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import Grin.Grin
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (HPTProgram(..), HasDataFlowInfo(..), DataFlowInfo)

type CG s a = ExceptT String (State s) a

data Result s
  = R IR.Reg
  | Z
  | A CPat (CG s (Result s))

emit :: HasDataFlowInfo s => IR.Instruction -> CG s ()
emit inst = modify' $ modifyInfo $ \dfi@HPTProgram{..} -> dfi {hptInstructions = inst : hptInstructions}

getsDfi :: (HasDataFlowInfo s, MonadState s m) => (DataFlowInfo -> a) -> m a
getsDfi f = gets (f . getDataFlowInfo)

stateDfi :: (HasDataFlowInfo s, MonadState s m) =>
            (DataFlowInfo -> (a, DataFlowInfo)) -> m a
stateDfi f = do
  (res, dfi) <- getsDfi f
  modify $ modifyInfo $ const dfi
  return res

-- creates regsiters for function arguments and result
getOrAddFunRegs :: HasDataFlowInfo s => Name -> Int -> CG s (IR.Reg, [IR.Reg])
getOrAddFunRegs name arity = do
  funMap <- getsDfi hptFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ modifyInfo $ \s@HPTProgram{..} -> s {hptFunctionArgMap = Map.insert name funRegs hptFunctionArgMap}
      pure funRegs

newReg :: HasDataFlowInfo s => CG s IR.Reg
newReg = stateDfi $ \s@HPTProgram{..} -> (IR.Reg hptRegisterCounter, s {hptRegisterCounter = succ hptRegisterCounter})

newMem :: HasDataFlowInfo s => CG s IR.Mem
newMem = stateDfi $ \s@HPTProgram{..} -> (IR.Mem hptMemoryCounter, s {hptMemoryCounter = succ hptMemoryCounter})

addReg :: HasDataFlowInfo s => Name -> IR.Reg -> CG s ()
addReg name reg = modify' $ modifyInfo $ \s@HPTProgram{..} -> s {hptRegisterMap = Map.insert name reg hptRegisterMap}

getReg :: HasDataFlowInfo s => Name -> CG s IR.Reg
getReg name = do
  regMap <- getsDfi hptRegisterMap
  case Map.lookup name regMap of
    Nothing   -> throwE $ "unknown variable " ++ name
    Just reg  -> pure reg

getTag :: HasDataFlowInfo s => Tag -> CG s IR.Tag
getTag tag = do
  tagMap <- getsDfi hptTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ modifyInfo $ \s -> s {hptTagMap = Bimap.insert tag t tagMap}
      pure t

codeGenBlock :: HasDataFlowInfo s => CG s () -> CG s [IR.Instruction]
codeGenBlock genM = do
  instructions <- stateDfi $ \s@HPTProgram{..} -> (hptInstructions, s {hptInstructions = []})
  genM
  blockInstructions <- stateDfi $ \s@HPTProgram{..} -> (reverse hptInstructions, s {hptInstructions = instructions})
  pure blockInstructions
