module LiveVariable.Tests.SumOpt where

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


sumOptSrc :: FilePath
sumOptSrc = lvaExamples </> "sum_opt.grin"

sumOptSpec :: LVAResult -> Spec
sumOptSpec found = it "sum_opt" $ found `sameAs` sumOptExpected

sumOptExpected :: LVAResult
sumOptExpected = LVAResult
  { _memory   = sumOptExpectedHeap
  , _register = sumOptExpectedRegisters
  , _function = sumOptExpectedFunctions
  }

sumOptExpectedHeap :: Vector Liveness
sumOptExpectedHeap = V.fromList []

sumOptExpectedRegisters :: Map Name Liveness
sumOptExpectedRegisters = M.fromList
  [ ("n13", liveVal)
  , ("n29", liveVal)
  , ("n29", liveVal)
  , ("n30", liveVal)
  , ("n31", liveVal)
  , ("b2",  liveVal)
  , ("n18",  liveVal)
  , ("n28",  liveVal)
  ]

sumOptExpectedFunctions :: Map Name (Liveness, Vector Liveness)
sumOptExpectedFunctions = mkFunctionLivenessMap
  [ ("sum", fun (liveVal, [liveVal, liveVal, liveVal]))
  , ("_prim_int_add", fun (liveVal, [liveVal, liveVal]))
  , ("_prim_int_gt",  fun (liveVal, [liveVal, liveVal]))
  , ("_prim_int_print", fun (liveVal, [liveVal]))
  ]
