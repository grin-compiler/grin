module DeadCodeElimination.Tests.DeadVariable.ReplaceUnspecLoc where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(replaceUnspecLocBefore, replaceUnspecLocAfter, replaceUnspecLocSpec) = mkDVETestCase "replace_unspec_loc"
