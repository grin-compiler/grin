module DeadCodeElimination.Tests.DeadVariable.ReplacePure where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(replacePureBefore, replacePureAfter, replacePureSpec) = mkDVETestCase "replace_pure"
