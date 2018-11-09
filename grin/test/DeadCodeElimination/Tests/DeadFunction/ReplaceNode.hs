module DeadCodeElimination.Tests.DeadFunction.ReplaceNode where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadFunction.Util

(replaceNodeBefore, replaceNodeAfter, replaceNodeSpec) = mkDFETestCase "replace_node"
