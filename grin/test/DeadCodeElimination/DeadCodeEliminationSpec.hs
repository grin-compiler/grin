module DeadCodeElimination.DeadCodeEliminationSpec where

import System.FilePath

import Test.IO
import Test.Hspec

import qualified DeadCodeElimination.Tests.DeadVariable.Spec as DVE

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = do
  DVE.runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = do
  DVE.runTestsFrom stackTest
