module DeadCodeElimination.Tests.DeadFunction.Util
  ( module DeadCodeElimination.Tests.DeadFunction.Util
  , module DeadCodeElimination.Tests.Util
  , module Test.Util
  ) where

import System.FilePath

import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin

import Test.Util
import Test.Hspec

import AbstractInterpretation.CByUtil

import DeadCodeElimination.Tests.Util


dfeBefore :: FilePath
dfeBefore = dceExamples </> "dead_fun" </> "before"

dfeAfter :: FilePath
dfeAfter = dceExamples </> "dead_fun" </> "after"

-- name ~ name of the test case, and also the grin source file
mkDFETestCase :: String -> (FilePath, FilePath, FilePath -> Exp -> Spec)
mkDFETestCase name = mkBeforeAfterTestCase name dfeBefore dfeAfter
