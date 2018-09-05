module LiveVariable.Tests.CaseAnonymous where

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


caseAnonymousSrc :: FilePath
caseAnonymousSrc = lvaExamples </> "case_anonymous.grin"

caseAnonymousSpec :: LVAResult -> Spec
caseAnonymousSpec found = it "case_anonymous" $ found `sameAs` caseAnonymousExpected

caseAnonymousExpected :: LVAResult
caseAnonymousExpected = LVAResult
  { _memory   = caseAnonymousExpectedHeap
  , _register = caseAnonymousExpectedRegisters
  , _function = caseAnonymousExpectedFunctions
  }

caseAnonymousExpectedHeap :: Vector Liveness
caseAnonymousExpectedHeap = V.fromList []

caseAnonymousExpectedRegisters :: Map Name Liveness
caseAnonymousExpectedRegisters = M.fromList
  [ ("a0", deadVal)
  , ("c0", liveVal)
  , ("c1", deadVal)
  ]

caseAnonymousExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseAnonymousExpectedFunctions = M.fromList
  [ ("grinMain", fun (nodeSet [ (cBool, [live]) ], [])) ]
