{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AbstractInterpretation.BinaryResult where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad

import AbstractInterpretation.IR
--import AbstractInterpretation.Reduce (NodeSet(..), Value(..), ComputerState(..), AbstractInterpretationResult(..))
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import Control.DeepSeq


------------
newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set Int32))}
  deriving (Eq, Show, Generic, NFData)

data Value
  = Value
  { _simpleType :: !(Set Int32)
  , _nodeSet    :: !(NodeSet)
  }
  deriving (Eq, Show, Generic, NFData)

data ComputerState
  = ComputerState
  { _memory    :: !(Vector NodeSet)
  , _register  :: !(Vector Value)
  }
  deriving (Eq, Show, Generic, NFData)

data AbstractInterpretationResult
  = AbsIntResult
  { _airComp :: !ComputerState
  , _airIter :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

------------
-- decoder

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

-- encoder

writeTag :: Tag -> Put
writeTag (Tag t) = putWord32le t

writeIntSet :: Set Int32 -> Put
writeIntSet s = do
  putInt32le 1000
  putInt32le . fromIntegral $ Set.size s
  mapM_ (putInt32le . fromIntegral) s

writeNodeItem :: Vector (Set Int32) -> Put
writeNodeItem v = do
  putInt32le 1001
  putInt32le . fromIntegral $ V.length v
  mapM_ writeIntSet v

writeNodeSet :: NodeSet -> Put
writeNodeSet (NodeSet ns) = do
  putInt32le 1002
  putInt32le . fromIntegral $ Map.size ns
  forM_ (Map.toList ns) $ \(t,i) -> do
    writeTag t
    writeNodeItem i

writeValue :: Value -> Put
writeValue (Value t n) = do
  putInt32le 1003
  writeIntSet t
  writeNodeSet n

writeAbstractInterpretationResult :: AbstractInterpretationResult -> Put
writeAbstractInterpretationResult r = do
  putInt32le . fromIntegral $ _airIter r
  putInt32le . fromIntegral . V.length . _memory $ _airComp r
  putInt32le . fromIntegral . V.length . _register $ _airComp r
  mapM_ writeNodeSet $ _memory $ _airComp r
  mapM_ writeValue $ _register $ _airComp r

instance Binary AbstractInterpretationResult where
  put = writeAbstractInterpretationResult
  get = readAbstractInterpretationResult
