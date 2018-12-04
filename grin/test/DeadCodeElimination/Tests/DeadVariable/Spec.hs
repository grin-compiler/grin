module DeadCodeElimination.Tests.DeadVariable.Spec where

import System.FilePath

import Data.Either (fromRight)

import Test.IO
import Test.Hspec

import Grin.Grin
import Grin.TypeCheck

import AbstractInterpretation.LVAResultTypes
import Transformations.Optimising.DeadVariableElimination
import Transformations.EffectMap

import LiveVariable.LiveVariableSpec (calcLiveness)
import DeadCodeElimination.Tests.Util


dveBefore :: FilePath
dveBefore = dceExamples </> "dead_variable" </> "before"

dveAfter :: FilePath
dveAfter = dceExamples </> "dead_variable" </> "after"


-- name ~ name of the test case, and also the grin source file
mkDVETestCase :: String -> (FilePath, FilePath, FilePath -> Exp -> Spec)
mkDVETestCase name = mkBeforeAfterTestCase name dveBefore dveAfter

(appSideEffect1Before, appSideEffect1After, appSideEffect1Spec) = mkDVETestCase "app_side_effect_1"
(appSideEffect2Before, appSideEffect2After, appSideEffect2Spec) = mkDVETestCase "app_side_effect_2"
(appSimpleBefore, appSimpleAfter, appSimpleSpec) = mkDVETestCase "app_simple"
(heapBefore, heapAfter, heapSpec) = mkDVETestCase "heap"
(patternMatchBefore, patternMatchAfter, patternMatchSpec) = mkDVETestCase "pattern_match"
(replaceAppBefore, replaceAppAfter, replaceAppSpec) = mkDVETestCase "replace_app"
(replaceCaseBefore, replaceCaseAfter, replaceCaseSpec) = mkDVETestCase "replace_case"
(replaceCaseRecBefore, replaceCaseRecAfter, replaceCaseRecSpec) = mkDVETestCase "replace_case_rec"
(replacePureBefore, replacePureAfter, replacePureSpec) = mkDVETestCase "replace_pure"
(replaceStoreBefore, replaceStoreAfter, replaceStoreSpec) = mkDVETestCase "replace_store"
(replaceUnspecLocBefore, replaceUnspecLocAfter, replaceUnspecLocSpec) = mkDVETestCase "replace_unspec_loc"
(replaceUpdateBefore, replaceUpdateAfter, replaceUpdateSpec) = mkDVETestCase "replace_update"
(simpleBefore, simpleAfter, simpleSpec) = mkDVETestCase "simple"
(trueSideEffectMinBefore, trueSideEffectMinAfter, trueSideEffectMinSpec) = mkDVETestCase "true_side_effect_min"
(updateBefore, updateAfter, updateSpec) = mkDVETestCase "update"


spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

dveTestName :: String
dveTestName = "Dead Variable Elimination"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = do
  testGroup dveTestName $
    mkBeforeAfterSpecFrom fromCurDir eliminateDeadVariables
      [ simpleBefore
      , heapBefore
      , updateBefore
      , appSimpleBefore
      , appSideEffect1Before
      , appSideEffect2Before
      , patternMatchBefore
      , replaceAppBefore
      , replaceCaseBefore
      , replaceCaseRecBefore
      , replacePureBefore
      , replaceStoreBefore
      , replaceUpdateBefore
      , replaceUnspecLocBefore
      , trueSideEffectMinBefore
      ]
      [ simpleAfter
      , heapAfter
      , updateAfter
      , appSimpleAfter
      , appSideEffect1After
      , appSideEffect2After
      , patternMatchAfter
      , replaceAppAfter
      , replaceCaseAfter
      , replaceCaseRecAfter
      , replacePureAfter
      , replaceStoreAfter
      , replaceUpdateAfter
      , replaceUnspecLocAfter
      , trueSideEffectMinAfter
      ]
      [ simpleSpec
      , heapSpec
      , updateSpec
      , appSimpleSpec
      , appSideEffect1Spec
      , appSideEffect2Spec
      , patternMatchSpec
      , replaceAppSpec
      , replaceCaseSpec
      , replaceCaseRecSpec
      , replacePureSpec
      , replaceStoreSpec
      , replaceUpdateSpec
      , replaceUnspecLocSpec
      , trueSideEffectMinSpec
      ]

eliminateDeadVariables :: Exp -> Exp
eliminateDeadVariables e =
  fromRight fail
  . deadVariableElimination lvaResult effMap tyEnv
  $ e
  where
    fail = error "Dead variable elimination failed. See the error logs for more information"
    lvaResult = calcLiveness e
    tyEnv = inferTypeEnv e
    effMap = effectMap (tyEnv, e)
