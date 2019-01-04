{-# LANGUAGE LambdaCase, RecordWildCards, RankNTypes #-}
module AbstractInterpretation.LiveVariable.CodeGenBase where

import Data.Int
import Data.Word
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec

import Control.Monad.State
import Control.Monad.Trans.Except

import Grin.Grin
import Grin.TypeEnvDefs
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (AbstractProgram(..))

type CG = State AbstractProgram

data Result
  = R IR.Reg
  | Z
  | A CPat (CG Result)

emit :: IR.Instruction -> CG ()
emit inst = modify' $ \s@AbstractProgram{..} -> s {_absInstructions = inst : _absInstructions}

-- creates regsiters for function arguments and result
getOrAddFunRegs :: Name -> Int -> CG (IR.Reg, [IR.Reg])
getOrAddFunRegs name arity = do
  funMap <- gets _absFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ \s@AbstractProgram{..} -> s {_absFunctionArgMap = Map.insert name funRegs _absFunctionArgMap}
      pure funRegs

newReg :: CG IR.Reg
newReg = state $ \s@AbstractProgram{..} -> (IR.Reg _absRegisterCounter, s {_absRegisterCounter = succ _absRegisterCounter})

newMem :: CG IR.Mem
newMem = state $ \s@AbstractProgram{..} -> (IR.Mem _absMemoryCounter, s {_absMemoryCounter = succ _absMemoryCounter})

addReg :: Name -> IR.Reg -> CG ()
addReg name reg = modify' $ \s@AbstractProgram{..} -> s {_absRegisterMap = Map.insert name reg _absRegisterMap}

getReg :: Name -> CG IR.Reg
getReg name = do
  regMap <- gets _absRegisterMap
  case Map.lookup name regMap of
    Nothing   -> error $ "unknown variable " ++ unpackName name
    Just reg  -> pure reg

getTag :: Tag -> CG IR.Tag
getTag tag = do
  tagMap <- gets _absTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ \s -> s {_absTagMap = Bimap.insert tag t tagMap}
      pure t

codeGenBlock :: CG a -> CG (a,[IR.Instruction])
codeGenBlock genM = do
  instructions <- state $ \s@AbstractProgram{..} -> (_absInstructions, s {_absInstructions = []})
  ret <- genM
  blockInstructions <- state $ \s@AbstractProgram{..} -> (reverse _absInstructions, s {_absInstructions = instructions})
  pure (ret, blockInstructions)

codeGenBlock_ :: CG a -> CG [IR.Instruction]
codeGenBlock_ = fmap snd . codeGenBlock

codeGenSimpleType :: SimpleType -> CG IR.Reg
codeGenSimpleType = \case
  T_Unit                -> newRegWithSimpleType (-1)
  T_Int64               -> newRegWithSimpleType (-2)
  T_Word64              -> newRegWithSimpleType (-3)
  T_Float               -> newRegWithSimpleType (-4)
  T_Bool                -> newRegWithSimpleType (-5)
  T_String              -> newRegWithSimpleType (-6)
  T_Char                -> newRegWithSimpleType (-7)
  T_UnspecifiedLocation -> newRegWithSimpleType (-8)
  T_Location locs -> do
    r <- newReg
    let locs' = map fromIntegral locs
    mapM_ (`extendSimpleType` r) locs'
    pure r
  t -> newReg
  where
  -- TODO: rename simple type to something more generic,
  newRegWithSimpleType :: IR.SimpleType -> CG IR.Reg
  newRegWithSimpleType irTy = newReg >>= extendSimpleType irTy

  -- TODO: rename simple type to something more generic,
  extendSimpleType :: IR.SimpleType -> IR.Reg -> CG IR.Reg
  extendSimpleType irTy r = do
    emit IR.Set
      { dstReg    = r
      , constant  = IR.CSimpleType irTy
      }
    pure r

codeGenNodeSetWith :: (Tag -> Vector SimpleType -> CG IR.Reg) ->
                      NodeSet -> CG IR.Reg
codeGenNodeSetWith cgNodeTy ns = do
  let (tags, argss) = unzip . Map.toList $ ns
  dst <- newReg
  nodeRegs <- zipWithM cgNodeTy tags argss
  forM_ nodeRegs $ \src -> emit IR.Move { srcReg = src, dstReg = dst }
  pure dst

-- Generate a node type from type information,
-- but preserve the first field for tag information.
codeGenTaggedNodeType :: Tag -> Vector SimpleType -> CG IR.Reg
codeGenTaggedNodeType tag ts = do
  let ts' = Vec.toList ts
  r <- newReg
  irTag <- getTag tag
  argRegs <- mapM codeGenSimpleType ts'
  emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length argRegs + 1)}
  forM_ (zip [1..] argRegs) $ \(idx, argReg) ->
    emit IR.Extend {srcReg = argReg, dstSelector = IR.NodeItem irTag idx, dstReg = r}
  pure r

-- FIXME: the following type signature is a bad oman ; it's not intuitive ; no-go ; refactor!
codeGenType :: (SimpleType -> CG IR.Reg) ->
               (NodeSet -> CG IR.Reg) ->
               Type -> CG IR.Reg
codeGenType cgSimpleTy cgNodeTy = \case
  T_SimpleType t -> cgSimpleTy t
  T_NodeSet   ns -> cgNodeTy ns

isPointer :: IR.Predicate
isPointer = IR.ValueIn (IR.Range 0 (maxBound :: Int32))

isNotPointer :: IR.Predicate
isNotPointer = IR.ValueIn (IR.Range (minBound :: Int32) 0)

-- For simple types, copies only pointer information
-- For nodes, copies the structure and the pointer information in the fields
copyStructureWithPtrInfo :: IR.Reg -> IR.Reg -> IR.Instruction
copyStructureWithPtrInfo srcReg dstReg = IR.ConditionalMove
  { srcReg    = srcReg
  , predicate = isPointer
  , dstReg    = dstReg
  }
