module LiveVariable.Tests.FunctionCall1 where

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


functionCall1Src :: FilePath
functionCall1Src = lvaExamples </> "function_call_1.grin"

functionCall1Spec :: LVAResult -> Spec
functionCall1Spec found = it "function_call_1" $ found `sameAs` functionCall1Expected

functionCall1Expected :: LVAResult
functionCall1Expected = LVAResult
  { _memory   = functionCall1ExpectedHeap
  , _register = functionCall1ExpectedRegisters
  , _function = functionCall1ExpectedFunctions
  }

functionCall1ExpectedHeap :: Vector Liveness
functionCall1ExpectedHeap = V.fromList []

functionCall1ExpectedRegisters :: Map Name Liveness
functionCall1ExpectedRegisters = M.fromList
  [ ("n",  livenessN)
  , ("y",  liveVal)
  , ("c0", liveVal)
  , ("c1", deadVal)
  , ("x",  livenessX)
  ]
  where livenessN = nodeSet [ (cFoo, [live]) ]

functionCall1ExpectedFunctions :: Map Name (Liveness, Vector Liveness)
functionCall1ExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (liveVal, [livenessX])) ]

livenessX :: Liveness
livenessX = nodeSet [ (cFoo, [live])
                    ]
