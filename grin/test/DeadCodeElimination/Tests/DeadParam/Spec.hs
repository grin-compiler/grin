module DeadCodeElimination.Tests.DeadParam.Spec where

import System.FilePath

import Data.Either (fromRight)

import Test.IO
import Test.Hspec

import Grin.Grin
import Grin.TypeCheck

import AbstractInterpretation.LVAResultTypes
import Transformations.Optimising.DeadParameterElimination

import LiveVariable.LiveVariableSpec (calcLiveness)
import DeadCodeElimination.Tests.Util


dpeBefore :: FilePath
dpeBefore = dceExamples </> "dead_param" </> "before"

dpeAfter :: FilePath
dpeAfter = dceExamples </> "dead_param" </> "after"

-- name ~ name of the test case, and also the grin source file
mkDPETestCase :: String -> (FilePath, FilePath, FilePath -> Exp -> Spec)
mkDPETestCase name = mkBeforeAfterTestCase name dpeBefore dpeAfter

(fNodeBefore, fNodeAfter, fNodeSpec) = mkDPETestCase "fnode"
(mutuallyRecursiveBefore, mutuallyRecursiveAfter, mutuallyRecursiveSpec) = mkDPETestCase "mutually_recursive"
(pNodeBefore, pNodeAfter, pNodeSpec) = mkDPETestCase "pnode"
(pNodeOptBefore, pNodeOptAfter, pNodeOptSpec) = mkDPETestCase "pnode_opt"
(simpleBefore, simpleAfter, simpleSpec) = mkDPETestCase "simple"


spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

dpeTestName :: String
dpeTestName = "Dead Parameter Elimination"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = do
  testGroup dpeTestName $
    mkBeforeAfterSpecFrom fromCurDir eliminateDeadParams
      [ fNodeBefore
      , pNodeBefore
      , pNodeOptBefore
      , simpleBefore
      , mutuallyRecursiveBefore
      ]
      [ fNodeAfter
      , pNodeAfter
      , pNodeOptAfter
      , simpleAfter
      , mutuallyRecursiveAfter
      ]
      [ fNodeSpec
      , pNodeSpec
      , pNodeOptSpec
      , simpleSpec
      , mutuallyRecursiveSpec
      ]

eliminateDeadParams :: Exp -> Exp
eliminateDeadParams e =
  fromRight fail
  . deadParameterElimination lvaResult tyEnv
  $ e
  where
    fail = error "Dead parameter elimination failed. See the error logs for more information"
    lvaResult = calcLiveness e
    tyEnv = inferTypeEnv e
