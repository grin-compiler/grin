module DeadCodeElimination.DeadCodeEliminationSpec where

import System.FilePath

import Test.IO
import Test.Hspec

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = pure ()

runTestsGHCi :: IO ()
runTestsGHCi = pure ()
