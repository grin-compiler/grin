{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.ExtendedSyntax.BinaryResult where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS

import Control.Monad

import AbstractInterpretation.ExtendedSyntax.IR
import AbstractInterpretation.ExtendedSyntax.Reduce (NodeSet(..), Value(..), ComputerState(..), AbstractInterpretationResult(..))

checkTag :: Int32 -> String -> Get ()
checkTag tag msg = do
  i <- getInt32le
  when (i /= tag) $ fail msg

readTag :: Get Tag
readTag = Tag <$> getWord32le

readIntSet :: Get (Set Int32)
readIntSet = do
  checkTag 1000 "int set expected"
  size <- fromIntegral <$> getInt32le
  Set.fromList <$> replicateM size getInt32le

readNodeItem :: Get (Vector (Set Int32))
readNodeItem = do
  checkTag 1001 "node item expected"
  size <- fromIntegral <$> getInt32le
  V.fromList <$> replicateM size readIntSet

readNodeSet :: Get NodeSet
readNodeSet = do
  checkTag 1002 "node set expected"
  size <- fromIntegral <$> getInt32le
  NodeSet . Map.fromList <$> replicateM size ((,) <$> readTag <*> readNodeItem)

readValue :: Get Value
readValue = do
  checkTag 1003 "value expected"
  Value <$> readIntSet <*> readNodeSet

readAbstractInterpretationResult :: Get AbstractInterpretationResult
readAbstractInterpretationResult = do
  iterCount <- fromIntegral <$> getInt32le
  memCount <- fromIntegral <$> getInt32le
  regCount <- fromIntegral <$> getInt32le
  mem <- V.fromList <$> replicateM memCount readNodeSet
  reg <- V.fromList <$> replicateM regCount readValue
  pure $ AbsIntResult
    { _airComp  = ComputerState {_memory = mem, _register = reg}
    , _airIter  = iterCount
    }


loadAbstractInterpretationResult :: String -> IO AbstractInterpretationResult
loadAbstractInterpretationResult fname = do
  runGet readAbstractInterpretationResult <$> LBS.readFile fname
