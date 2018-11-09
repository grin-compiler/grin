module DeadCodeElimination.Tests.DeadFunction.ReplaceSimpleType where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadFunction.Util

(replaceSimpleTypeBefore, replaceSimpleTypeAfter, replaceSimpleTypeSpec) = mkDFETestCase "replace_simple_type"
