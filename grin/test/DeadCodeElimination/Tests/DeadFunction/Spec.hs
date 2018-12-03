module DeadCodeElimination.Tests.DeadFunction.Spec where

import System.FilePath

import Data.Either (fromRight)

import Test.IO
import Test.Hspec
import Test.Util

import Grin.Grin
import Grin.TypeCheck

import AbstractInterpretation.LVAResultTypes
import Transformations.Optimising.DeadFunctionElimination
import Transformations.EffectMap

import LiveVariable.LiveVariableSpec (calcLiveness)
import DeadCodeElimination.Tests.Util


dfeBefore :: FilePath
dfeBefore = dceExamples </> "dead_fun" </> "before"

dfeAfter :: FilePath
dfeAfter = dceExamples </> "dead_fun" </> "after"

-- name ~ name of the test case, and also the grin source file
mkDFETestCase :: String -> (FilePath, FilePath, FilePath -> Exp -> Spec)
mkDFETestCase name = mkBeforeAfterTestCase name dfeBefore dfeAfter

(appSideEffect1Before, appSideEffect1After, appSideEffect1Spec) = mkDFETestCase "app_side_effect_1"
(simpleBefore, simpleAfter, simpleSpec) = mkDFETestCase "simple"
(replaceSimpleTypeBefore, replaceSimpleTypeAfter, replaceSimpleTypeSpec) = mkDFETestCase "replace_simple_type"
(replaceNodeBefore, replaceNodeAfter, replaceNodeSpec) = mkDFETestCase "replace_node"
(mutuallyRecursiveBefore, mutuallyRecursiveAfter, mutuallyRecursiveSpec) = mkDFETestCase "mutually_recursive"
(trueSideEffectMinBefore, trueSideEffectMinAfter, trueSideEffectMinSpec) = mkDFETestCase "true_side_effect_min"


spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

dfeTestName :: String
dfeTestName = "Dead Function Elimination"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = do
  testGroup dfeTestName $
    mkBeforeAfterSpecFrom fromCurDir eliminateDeadFunctions
      [ appSideEffect1Before
      , mutuallyRecursiveBefore
      , replaceNodeBefore
      , replaceSimpleTypeBefore
      , simpleBefore
      , trueSideEffectMinBefore
      ]
      [ appSideEffect1After
      , mutuallyRecursiveAfter
      , replaceNodeAfter
      , replaceSimpleTypeAfter
      , simpleAfter
      , trueSideEffectMinAfter
      ]
      [ appSideEffect1Spec
      , mutuallyRecursiveSpec
      , replaceNodeSpec
      , replaceSimpleTypeSpec
      , simpleSpec
      , trueSideEffectMinSpec
      ]

eliminateDeadFunctions :: Exp -> Exp
eliminateDeadFunctions e =
  fromRight fail
  . deadFunctionElimination lvaResult effMap tyEnv
  $ e
  where
    fail = error "Dead function elimination failed. See the error logs for more information"
    lvaResult = calcLiveness e
    tyEnv = inferTypeEnv e
    effMap = effectMap (tyEnv, e)
