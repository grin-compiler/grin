module HeapPointsTo.HeapPointsToSpec where

import System.FilePath

import Grin.Grin

import Test.IO
import Test.Hspec

import AbstractInterpretation.Reduce
import AbstractInterpretation.HeapPointsTo
import AbstractInterpretation.HPTResult

import HeapPointsTo.Tests.Undefined

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = runTestsFromWith fromCurDir calcHPTResult
  [ undefinedSrc
  ]
  [ undefinedSpec
  ]

calcHPTResult :: Exp -> HPTResult
calcHPTResult prog
  | Right hptProgram <- codeGen prog
  , computer <- evalDataFlowInfo hptProgram
  = toHPTResult hptProgram computer