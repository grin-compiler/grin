module DeadCodeElimination.Tests.DeadVariable.PatternMatch where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(patternMatchBefore, patternMatchAfter, patternMatchSpec) = mkDVETestCase "pattern_match"
