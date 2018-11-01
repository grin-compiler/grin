module DeadCodeElimination.Tests.DeadVariable.AppSimple where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(appSimpleBefore, appSimpleAfter, appSimpleSpec) = mkDVETestCase "app_simple"
