module DeadCodeElimination.DeadCodeEliminationSpec where

import System.FilePath

import Test.IO
import Test.Hspec

import qualified DeadCodeElimination.Tests.DeadFunction.Spec as DFE
import qualified DeadCodeElimination.Tests.DeadParam.Spec as DPE
import qualified DeadCodeElimination.Tests.DeadVariable.Spec as DVE

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = do
  DFE.runTestsFrom stackRoot
  DPE.runTestsFrom stackRoot
  DVE.runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = do
  DFE.runTestsFrom stackTest
  DPE.runTestsFrom stackTest
  DVE.runTestsFrom stackTest
