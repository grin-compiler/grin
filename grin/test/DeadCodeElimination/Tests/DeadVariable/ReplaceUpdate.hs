module DeadCodeElimination.Tests.DeadVariable.ReplaceUpdate where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(replaceUpdateBefore, replaceUpdateAfter, replaceUpdateSpec) = mkDVETestCase "replace_update"
