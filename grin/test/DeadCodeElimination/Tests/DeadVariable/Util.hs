module DeadCodeElimination.Tests.DeadVariable.Util
  ( module DeadCodeElimination.Tests.DeadVariable.Util
  , module DeadCodeElimination.Tests.Util
  , module Test.Util
  ) where

import System.FilePath

import Grin.Grin

import Test.Util
import Test.Hspec

import DeadCodeElimination.Tests.Util


dveBefore :: FilePath
dveBefore = dceExamples </> "dead_variable" </> "before"

dveAfter :: FilePath
dveAfter = dceExamples </> "dead_variable" </> "after"


-- name ~ name of the test case, and also the grin source file
mkDVETestCase :: String -> (FilePath, FilePath, FilePath -> Exp -> Spec)
mkDVETestCase name = mkBeforeAfterTestCase name dveBefore dveAfter
