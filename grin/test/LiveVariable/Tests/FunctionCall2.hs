module LiveVariable.Tests.FunctionCall2 where

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


functionCall2Src :: FilePath
functionCall2Src = lvaExamples </> "function_call_2.grin"

functionCall2Spec :: LVAResult -> Spec
functionCall2Spec found = it "function_call_2" $ found `sameAs` functionCall2Expected

functionCall2Expected :: LVAResult
functionCall2Expected = LVAResult
  { _memory   = functionCall2ExpectedHeap
  , _register = functionCall2ExpectedRegisters
  , _function = functionCall2ExpectedFunctions
  }

functionCall2ExpectedHeap :: Vector Liveness
functionCall2ExpectedHeap = V.fromList []

functionCall2ExpectedRegisters :: Map Name Liveness
functionCall2ExpectedRegisters = M.fromList
  [ ("a0", liveVal)
  , ("a1", liveVal)
  , ("b0", deadVal)
  , ("b1", deadVal)
  , ("n",  livenessN)
  ]
  where livenessN = nodeSet [ (cOne, [live]) ]

functionCall2ExpectedFunctions :: Map Name (Liveness, Vector Liveness)
functionCall2ExpectedFunctions = M.fromList
  [ ("f",        fun (nodeSet [ (cTwo, [live, dead]) ], []))
  , ("grinMain", fun (nodeSet [ (cOne, [live]) ], [])) ]
