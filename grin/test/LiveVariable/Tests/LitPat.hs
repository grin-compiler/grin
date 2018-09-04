module LiveVariable.Tests.LitPat where

import Data.Map    (Map)
import Data.Vector (Vector)

import qualified Data.Map    as M
import qualified Data.Vector as V

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import AbstractInterpretation.LiveVariable
import AbstractInterpretation.LVAResult

import LiveVariable.Tests.Util


litPatSrc :: FilePath
litPatSrc = lvaExamples </> "lit_pat.grin"

litPatSpec :: LVAResult -> Spec
litPatSpec found = it "lit_pat" $ found `sameAs` litPatExpected

litPatExpected :: LVAResult
litPatExpected = LVAResult
  { _memory   = litPatExpectedHeap
  , _register = litPatExpectedRegisters
  , _function = litPatExpectedFunctions
  }

litPatExpectedHeap :: Vector Liveness
litPatExpectedHeap = V.fromList []

litPatExpectedRegisters :: Map Name Liveness
litPatExpectedRegisters = M.fromList
  [ ("y", liveVal)
  , ("x", liveVal)
  ]

litPatExpectedFunctions :: Map Name (Liveness, Vector Liveness)
litPatExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (liveVal, [liveVal])) ]
