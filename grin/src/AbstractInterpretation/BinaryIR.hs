{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.BinaryIR (encodeAbstractProgram) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder

import AbstractInterpretation.IR

data Env
  = Env
  { envTagSets        :: ![Set Tag]
  , envTagSetCount    :: !Int
  , envInstCount      :: !Int -- program / if has effect on this
  , envBlocks         :: ![(Int,Int)]
  , envBlockCount     :: !Int
  , envBuilder        :: !Builder
  , envBuilderSuffix  :: !Builder
  }

emptyEnv = Env
  { envTagSets        = []
  , envTagSetCount    = 0
  , envInstCount      = 0
  , envBlocks         = []
  , envBlockCount     = 0
  , envBuilder        = mempty
  , envBuilderSuffix  = mempty
  }

type W = State Env

emit :: Builder -> W ()
emit b = modify' $ \env@Env{..} -> env {envBuilder = envBuilder <> b}

writeI32 :: Int32 -> W ()
writeI32 i = emit $ int32LE i

writeW32 :: Word32 -> W ()
writeW32 w = emit $ word32LE w

writeReg :: Reg -> W ()
writeReg (Reg r) = writeW32 r

writeMem :: Mem -> W ()
writeMem (Mem m) = writeW32 m

writeTagSet :: Set Tag -> W ()
writeTagSet s = do
  idx <- gets envTagSetCount
  modify' $ \env@Env{..} -> env {envTagSets = envTagSets ++ [s], envTagSetCount = succ idx}
  writeI32 $ fromIntegral idx

writeBlock :: [Instruction] -> W ()
writeBlock il = do
  let size = length il
  iCount <- gets envInstCount
  bCount <- gets envBlockCount
  modify' $ \env@Env{..} -> env {envInstCount = iCount + size, envBlockCount = succ bCount, envBlocks = envBlocks ++ [(iCount, size)]}
  writeI32 $ fromIntegral bCount
  currentBuilder <- gets envBuilder
  modify' $ \env@Env{..} -> env {envBuilder = mempty}
  mapM_ writeInstruction il
  blockBuilder <- gets envBuilder
  modify' $ \env@Env{..} -> env {envBuilder = currentBuilder, envBuilderSuffix = envBuilderSuffix <> blockBuilder}

-----------------------------------

writeRange :: Range -> W ()
writeRange Range{..} = do
  writeI32 from
  writeI32 to

writeType :: Int32 -> W ()
writeType = writeI32

writeTag :: Tag -> W ()
writeTag (Tag w) = writeW32 w

writePredicate :: Predicate -> W ()
writePredicate = \case
  TagIn s -> do
    writeType 100
    writeTagSet s
  TagNotIn s -> do
    writeType 101
    writeTagSet s
  ValueIn r -> do
    writeType 102
    writeRange r
  ValueNotIn r -> do
    writeType 103
    writeRange r

writeCondition :: Condition -> W ()
writeCondition = \case
  NodeTypeExists t -> do
    writeType 200
    writeTag t
  SimpleTypeExists st -> do
    writeType 201
    writeI32 st
  AnyNotIn s -> do
    writeType 202
    writeTagSet s
  Any p -> do
    writeType 203
    writePredicate p

writeSelector :: Selector -> W ()
writeSelector = \case
  NodeItem t i -> do
    writeType 300
    writeTag t
    writeI32 $ fromIntegral i
  ConditionAsSelector c -> do
    writeType 301
    writeCondition c
  AllFields -> do
    writeType 302

writeConstant :: Constant -> W ()
writeConstant = \case
  CSimpleType st -> do
    writeType 400
    writeI32 st
  CHeapLocation m -> do
    writeType 401
    writeMem m
  CNodeType t a -> do
    writeType 402
    writeTag t
    writeI32 $ fromIntegral a
  CNodeItem t i v -> do
    writeType 403
    writeTag t
    writeI32 $ fromIntegral i
    writeI32 v

writeInstruction :: Instruction -> W ()
writeInstruction = \case
  If {..} -> do
    writeType 500
    writeCondition condition
    writeReg srcReg
    writeBlock instructions
  Project {..} -> do
    writeType 501
    writeSelector srcSelector
    writeReg srcReg
    writeReg dstReg
  Extend {..} -> do
    writeType 502
    writeReg srcReg
    writeSelector dstSelector
    writeReg dstReg
  Move {..} -> do
    writeType 503
    writeReg srcReg
    writeReg dstReg
  RestrictedMove {..} -> do
    writeType 504
    writeReg srcReg
    writeReg dstReg
  ConditionalMove {..} -> do
    writeType 505
    writeReg srcReg
    writePredicate predicate
    writeReg dstReg
  Fetch {..} -> do
    writeType 506
    writeReg addressReg
    writeReg dstReg
  Store {..} -> do
    writeType 507
    writeReg srcReg
    writeMem address
  Update {..} -> do
    writeType 508
    writeReg srcReg
    writeReg addressReg
  RestrictedUpdate {..} -> do
    writeType 509
    writeReg srcReg
    writeReg addressReg
  ConditionalUpdate {..} -> do
    writeType 510
    writeReg srcReg
    writePredicate predicate
    writeReg addressReg
  Set {..} -> do
    writeType 511
    writeReg dstReg
    writeConstant constant

{-
  memory count    i32
  register count  i32
  start block id  i32
  cmd count       i32
  cmds ...
  block count     i32
  blocks (ranges) ...
  intset count  i32
    set size      i32
    set elems ... [i32]
-}

encodeAbstractProgram :: AbstractProgram -> LBS.ByteString
encodeAbstractProgram AbstractProgram {..} = toLazyByteString (envBuilder env) where
  env = flip execState emptyEnv $ do
    writeW32 _absMemoryCounter
    writeW32 _absRegisterCounter

    -- start block id
    writeBlock _absInstructions

    -- commands
    cmdCount <- gets envInstCount
    writeI32 $ fromIntegral cmdCount
    cmdsBin <- gets envBuilderSuffix
    emit cmdsBin

    -- bocks
    blkCount <- gets envBlockCount
    writeI32 $ fromIntegral blkCount
    blocks <- gets envBlocks
    forM_ blocks $ \(a,b) -> do
      writeI32 $ fromIntegral a
      writeI32 $ fromIntegral b

    -- intsets
    setCount <- gets envTagSetCount
    writeI32 $ fromIntegral setCount
    sets <- gets envTagSets
    forM_ sets $ \s -> do
      writeI32 $ fromIntegral $ Set.size s
      forM_ (Set.toList s) (\(Tag t) -> writeI32 $ fromIntegral t)
