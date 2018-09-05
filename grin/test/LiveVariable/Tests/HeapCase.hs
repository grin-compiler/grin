module LiveVariable.Tests.HeapCase where

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


heapCaseSrc :: FilePath
heapCaseSrc = lvaExamples </> "heap_case.grin"

heapCaseSpec :: LVAResult -> Spec
heapCaseSpec found = it "heap_case" $ found `sameAs` heapCaseExpected

heapCaseExpected :: LVAResult
heapCaseExpected = LVAResult
  { _memory   = heapCaseExpectedHeap
  , _register = heapCaseExpectedRegisters
  , _function = heapCaseExpectedFunctions
  }

heapCaseExpectedHeap :: Vector Liveness
heapCaseExpectedHeap = V.fromList
  [ nodeSet [ (cWordH, [dead]) ]
  , nodeSet [ (cBoolH, [live]) ]
  , deadLoc -- the heap location is generated, but the instructions aren't executed
  ]

heapCaseExpectedRegisters :: Map Name Liveness
heapCaseExpectedRegisters = M.fromList
  [ ("n0", livenessFRet)
  , ("p0", liveVal)
  , ("c0", deadVal)
  , ("c1", liveVal)
  , ("c2", deadVal) -- the register is generated, but the instructions aren't executed
  , ("c3", deadVal)
  , ("c4", liveVal)
  , ("c5", deadVal) -- the register is generated, but the instructions aren't executed
  , ("n1", livenessN1)
  , ("x",  liveVal)
  ]
  where livenessN1 = nodeSet [ (cBoolH, [live])
                             , (cWordH, [dead])
                             ]

heapCaseExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapCaseExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (livenessFRet,[liveVal])) ]

livenessLoc1 :: Liveness
livenessLoc1 = nodeSet [ (cBoolH, [live])
                       , (cWordH, [dead])
                       ]

livenessFRet :: Liveness
livenessFRet = nodeSet [ (cBool, [live])
                       , (cWord, [dead])
                       ]
