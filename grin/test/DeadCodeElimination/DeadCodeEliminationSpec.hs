module DeadCodeElimination.DeadCodeEliminationSpec where

import System.FilePath

import Test.IO
import Test.Hspec

import qualified DeadCodeElimination.Tests.DeadData.Spec as DDE
import qualified DeadCodeElimination.Tests.DeadParam.Spec as DPE

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = do 
  DDE.runTestsFrom stackRoot
  DPE.runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = do 
  DDE.runTestsFrom stackTest
  DPE.runTestsFrom stackTest