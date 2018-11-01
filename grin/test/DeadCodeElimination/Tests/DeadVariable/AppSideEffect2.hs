module DeadCodeElimination.Tests.DeadVariable.AppSideEffect2 where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(appSideEffect2Before, appSideEffect2After, appSideEffect2Spec) = mkDVETestCase "app_side_effect_2"
