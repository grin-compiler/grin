module LiveVariable.Tests.CaseRestricted where

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


caseRestrictedSrc :: FilePath
caseRestrictedSrc = lvaExamples </> "case_restricted.grin"

caseRestrictedSpec :: LVAResult -> Spec
caseRestrictedSpec found = it "case_restricted" $ found `sameAs` caseRestrictedExpected

caseRestrictedExpected :: LVAResult
caseRestrictedExpected = LVAResult
  { _memory   = caseRestrictedExpectedHeap
  , _register = caseRestrictedExpectedRegisters
  , _function = caseRestrictedExpectedFunctions
  }

caseRestrictedExpectedHeap :: Vector Liveness
caseRestrictedExpectedHeap = V.fromList []

caseRestrictedExpectedRegisters :: Map Name Liveness
caseRestrictedExpectedRegisters = M.fromList
  [ ("n0", livenessFRet)
  , ("c0", deadVal)
  , ("c1", deadVal)
  , ("b0", liveVal)
  , ("b1", liveVal)
  , ("x",  liveVal)
  ]

caseRestrictedExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseRestrictedExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (livenessFRet, [liveVal])) ]

livenessFRet :: Liveness
livenessFRet = nodeSet [ (cInt,  [live])
                       , (cWord, [live])
                       ]
