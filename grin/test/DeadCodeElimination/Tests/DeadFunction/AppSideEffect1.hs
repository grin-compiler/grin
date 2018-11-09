module DeadCodeElimination.Tests.DeadFunction.AppSideEffect1 where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadFunction.Util

(appSideEffect1Before, appSideEffect1After, appSideEffect1Spec) = mkDFETestCase "app_side_effect_1"
