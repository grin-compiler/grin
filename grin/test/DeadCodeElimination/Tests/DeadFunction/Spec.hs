module DeadCodeElimination.Tests.DeadFunction.Spec where

import System.FilePath

import Data.Either (fromRight)

import Test.IO
import Test.Hspec

import Grin.Grin
import Grin.TypeCheck

import AbstractInterpretation.LVAResultTypes
import Transformations.Optimising.DeadFunctionElimination
import Transformations.EffectMap

import LiveVariable.LiveVariableSpec (calcLiveness)

import DeadCodeElimination.Tests.DeadFunction.AppSideEffect1
import DeadCodeElimination.Tests.DeadFunction.MutuallyRecursive
import DeadCodeElimination.Tests.DeadFunction.ReplaceNode
import DeadCodeElimination.Tests.DeadFunction.ReplaceSimpleType
import DeadCodeElimination.Tests.DeadFunction.Simple
import DeadCodeElimination.Tests.DeadFunction.TrueSideEffectMin


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


