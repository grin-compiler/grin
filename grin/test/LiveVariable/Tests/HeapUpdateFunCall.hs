module LiveVariable.Tests.HeapUpdateFunCall where

import Data.Map    (Map)
import Data.Vector (Vector)

import qualified Data.Map    as M
import qualified Data.Vector as V

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import AbstractInterpretation.LiveVariable hiding (live)
import AbstractInterpretation.LVAResult

import LiveVariable.Tests.Util


heapUpdateFunCallSrc :: FilePath
heapUpdateFunCallSrc = lvaExamples </> "heap_update_fun_call.grin"

heapUpdateFunCallSpec :: LVAResult -> Spec
heapUpdateFunCallSpec found = it "heap_update_fun_call" $ found `sameAs` heapUpdateFunCallExpected

heapUpdateFunCallExpected :: LVAResult
heapUpdateFunCallExpected = LVAResult
  { _memory   = heapUpdateFunCallExpectedHeap
  , _register = heapUpdateFunCallExpectedRegisters
  , _function = heapUpdateFunCallExpectedFunctions
  }

heapUpdateFunCallExpectedHeap :: Vector Liveness
heapUpdateFunCallExpectedHeap = V.fromList
  [ nodeSet [ (cBool, [live])
            , (cNode, [live])
            ]
  , nodeSet [ (cWord, [dead])
            , (cNode, [live])
            ]
  ]

heapUpdateFunCallExpectedRegisters :: Map Name Liveness
heapUpdateFunCallExpectedRegisters = M.fromList
  [ ("n0", livenessN0)
  , ("n1", livenessN1)
  , ("n2", livenessN2)
  , ("n3", livenessN3)
  , ("p0", liveVal)
  , ("c0", liveVal)
  , ("c1", deadVal)
  , ("c2", liveVal)
  , ("c3", deadVal)
  , ("q0", deadVal)
  , ("x",  deadVal)
  , ("y",  livenessN0)
  , ("z",  livenessN1)
  , ("u",  liveVal)
  , ("v",  livenessN2)
  ]
  where livenessN3 = nodeSet [ (cBool, [live])
                             , (cWord, [dead])
                             , (cNode, [live])
                             ]

heapUpdateFunCallExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapUpdateFunCallExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (liveVal, [deadVal, livenessN0, livenessN1]))
  , ("g", fun (deadVal, [liveVal, livenessN2]))
  ]

livenessN0, livenessN1, livenessN2 :: Liveness
livenessN0 = nodeSet [ (cBool, [live]) ]
livenessN1 = nodeSet [ (cWord, [dead]) ]
livenessN2 = nodeSet [ (cNode, [live]) ]
