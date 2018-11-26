module LiveVariable.Tests.CaseMinLit where

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


caseMinLitSrc :: FilePath
caseMinLitSrc = lvaExamples </> "case_min_lit.grin"

caseMinLitSpec :: LVAResult -> Spec
caseMinLitSpec found = it "case_min_lit" $ found `sameAs` caseMinLitExpected

caseMinLitExpected :: LVAResult
caseMinLitExpected = LVAResult
  { _memory   = caseMinLitExpectedHeap
  , _register = caseMinLitExpectedRegisters
  , _function = caseMinLitExpectedFunctions
  }

caseMinLitExpectedHeap :: Vector Liveness
caseMinLitExpectedHeap = V.fromList []

caseMinLitExpectedRegisters :: Map Name Liveness
caseMinLitExpectedRegisters = M.fromList
  [ ("a", liveVal)
  , ("b", liveVal)
  , ("c", liveVal)
  , ("d", liveVal)
  , ("e", deadVal)
  ]

caseMinLitExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseMinLitExpectedFunctions = mkFunctionLivenessMap []
