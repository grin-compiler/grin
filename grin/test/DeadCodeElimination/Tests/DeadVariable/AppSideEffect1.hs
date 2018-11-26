module DeadCodeElimination.Tests.DeadVariable.AppSideEffect1 where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(appSideEffect1Before, appSideEffect1After, appSideEffect1Spec) = mkDVETestCase "app_side_effect_1"
