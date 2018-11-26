module LiveVariable.Tests.Fields where

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


fieldsSrc :: FilePath
fieldsSrc = lvaExamples </> "fields.grin"

fieldsSpec :: LVAResult -> Spec
fieldsSpec found = it "fields" $ found `sameAs` fieldsExpected

fieldsExpected :: LVAResult
fieldsExpected = LVAResult
  { _memory   = fieldsExpectedHeap
  , _register = fieldsExpectedRegisters
  , _function = fieldsExpectedFunctions
  }

fieldsExpectedHeap :: Vector Liveness
fieldsExpectedHeap = V.fromList []

fieldsExpectedRegisters :: Map Name Liveness
fieldsExpectedRegisters = M.fromList
  [ ("a0", deadVal)
  , ("a1", liveVal)
  , ("n0", livenessX)
  , ("c0", deadVal)
  , ("c1", liveVal)
  , ("c2", deadVal)
  , ("c3", liveVal)
  , ("n1", livenessX)
  , ("x",  livenessX)
  ]

fieldsExpectedFunctions :: Map Name (Liveness, Vector Liveness)
fieldsExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (liveVal, [livenessX])) ]

livenessX :: Liveness
livenessX = nodeSet [ (cNode, [dead, live]) ]
