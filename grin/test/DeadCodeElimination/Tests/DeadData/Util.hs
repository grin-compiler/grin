module DeadCodeElimination.Tests.DeadData.Util
  ( module DeadCodeElimination.Tests.DeadData.Util
  , module DeadCodeElimination.Tests.Util
  , module Test.Util
  ) where

import System.FilePath

import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin

import Test.Util
import Test.Hspec

import AbstractInterpretation.CByUtil

import DeadCodeElimination.Tests.Util


ddeBefore :: FilePath
ddeBefore = dceExamples </> "dead_data" </> "before"

ddeAfter :: FilePath
ddeAfter = dceExamples </> "dead_data" </> "after"

mkGraph :: [ (Name, [(Tag, [Name])]) ] -> ProducerGraph
mkGraph = toProducerGraph
        . Map.map (Map.map Set.fromList)
        . Map.map Map.fromList
        . Map.fromList


-- name ~ name of the test case, and also the grin source file
mkDDETestCase :: String -> (FilePath, FilePath, FilePath -> Exp -> Spec)
mkDDETestCase name = mkBeforeAfterTestCase name ddeBefore ddeAfter
