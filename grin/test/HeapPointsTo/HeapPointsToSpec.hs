module HeapPointsTo.HeapPointsToSpec where

import System.FilePath

import Grin.Grin

import Test.IO
import Test.Hspec

import AbstractInterpretation.Reduce
import AbstractInterpretation.HeapPointsTo
import AbstractInterpretation.HPTResult

import HeapPointsTo.Tests.Undefined
import HeapPointsTo.Tests.UnspecLoc

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

hptTestName :: String 
hptTestName = "Heap Points To"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = testGroup hptTestName $
  mkSpecFromWith fromCurDir calcHPTResult
    [ undefinedSrc
    , unspecLocSrc
    ]
    [ undefinedSpec
    , unspecLocSpec
    ]

calcHPTResult :: Exp -> HPTResult
calcHPTResult prog
  | Right hptProgram <- codeGen prog
  , computer <- _airComp . evalDataFlowInfo $ hptProgram
  = toHPTResult hptProgram computer