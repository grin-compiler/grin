module DeadCodeElimination.Tests.DeadVariable.ReplaceApp where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(replaceAppBefore, replaceAppAfter, replaceAppSpec) = mkDVETestCase "replace_app"
