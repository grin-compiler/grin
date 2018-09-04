module LiveVariable.Tests.CaseBackwardPropagation where

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


caseBackwardPropagationSrc :: FilePath
caseBackwardPropagationSrc = lvaExamples </> "case_backward_propagation.grin"

caseBackwardPropagationSpec :: LVAResult -> Spec
caseBackwardPropagationSpec found = it "case_backward_propagation" $ found `sameAs` caseBackwardPropagationExpected

caseBackwardPropagationExpected :: LVAResult
caseBackwardPropagationExpected = LVAResult
  { _memory   = caseBackwardPropagationExpectedHeap
  , _register = caseBackwardPropagationExpectedRegisters
  , _function = caseBackwardPropagationExpectedFunctions
  }

caseBackwardPropagationExpectedHeap :: Vector Liveness
caseBackwardPropagationExpectedHeap = V.fromList []

caseBackwardPropagationExpectedRegisters :: Map Name Liveness
caseBackwardPropagationExpectedRegisters = M.fromList
  [ ("n0", livenessFRet)
  , ("c0", deadVal)
  , ("c1", liveVal)
  , ("b0", liveVal)
  , ("b1", liveVal)
  , ("x",  liveVal)
  ]

caseBackwardPropagationExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseBackwardPropagationExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (livenessFRet, [liveVal])) ]

livenessFRet :: Liveness
livenessFRet = nodeSet [ (cInt,  [live])
                       , (cBool, [live])
                       , (cWord, [live])
                       ]
