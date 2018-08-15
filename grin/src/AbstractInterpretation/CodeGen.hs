{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections #-}
module AbstractInterpretation.CodeGen where

import Control.Monad.Trans.Except
import Data.Word
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import Grin.Grin
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (HPTProgram(..))

type CG = ExceptT String (State HPTProgram)

data Result
  = R IR.Reg
  | Z
  | A CPat (CG Result)


emit :: IR.Instruction -> CG ()
emit inst = modify' $ \s@HPTProgram{..} -> s {hptInstructions = inst : hptInstructions}

-- creates regsiters for function arguments and result
getOrAddFunRegs :: Name -> Int -> CG (IR.Reg, [IR.Reg])
getOrAddFunRegs name arity = do
  funMap <- gets hptFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ \s@HPTProgram{..} -> s {hptFunctionArgMap = Map.insert name funRegs hptFunctionArgMap}
      pure funRegs

newReg :: CG IR.Reg
newReg = state $ \s@HPTProgram{..} -> (IR.Reg hptRegisterCounter, s {hptRegisterCounter = succ hptRegisterCounter})

newMem :: CG IR.Mem
newMem = state $ \s@HPTProgram{..} -> (IR.Mem hptMemoryCounter, s {hptMemoryCounter = succ hptMemoryCounter})

addReg :: Name -> IR.Reg -> CG ()
addReg name reg = modify' $ \s@HPTProgram{..} -> s {hptRegisterMap = Map.insert name reg hptRegisterMap}

getReg :: Name -> CG IR.Reg
getReg name = do
  regMap <- gets hptRegisterMap
  case Map.lookup name regMap of
    Nothing   -> throwE $ "unknown variable " ++ name
    Just reg  -> pure reg

getTag :: Tag -> CG IR.Tag
getTag tag = do
  tagMap <- gets hptTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ \s -> s {hptTagMap = Bimap.insert tag t tagMap}
      pure t

codeGenBlock :: CG () -> CG [IR.Instruction]
codeGenBlock genM = do
  instructions <- state $ \s@HPTProgram{..} -> (hptInstructions, s {hptInstructions = []})
  genM
  blockInstructions <- state $ \s@HPTProgram{..} -> (reverse hptInstructions, s {hptInstructions = instructions})
  pure blockInstructions
