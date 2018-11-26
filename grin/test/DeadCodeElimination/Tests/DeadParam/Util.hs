module DeadCodeElimination.Tests.DeadParam.Util
  ( module DeadCodeElimination.Tests.DeadParam.Util
  , module DeadCodeElimination.Tests.Util
  , module Test.Util
  ) where

import System.FilePath

import Grin.Grin

import Test.Util
import Test.Hspec

import DeadCodeElimination.Tests.Util


dpeBefore :: FilePath
dpeBefore = dceExamples </> "dead_param" </> "before"

dpeAfter :: FilePath
dpeAfter = dceExamples </> "dead_param" </> "after"


-- name ~ name of the test case, and also the grin source file
mkDPETestCase :: String -> (FilePath, FilePath, FilePath -> Exp -> Spec)
mkDPETestCase name = mkBeforeAfterTestCase name dpeBefore dpeAfter
