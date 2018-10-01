module DeadCodeElimination.Tests.Util
  ( module DeadCodeElimination.Tests.Util
  , module Test.Util
  ) where

import System.FilePath

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin
import Grin.Parse

import Transformations.Optimising.DeadDataElimination

import Test.Util
import Test.Hspec
import Test.Assertions

import AbstractInterpretation.CByUtil
import AbstractInterpretation.CByResult (CByResult)
import AbstractInterpretation.LVAResult (LVAResult)


dceExamples :: FilePath
dceExamples = "DeadCodeElimination" </> "examples"

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
mkDDETestCase :: String -> (FilePath, FilePath, FilePath -> (LVAResult, CByResult, Exp) -> Spec)
mkDDETestCase name = (before, after, specFun)
  where before = ddeBefore </> name <.> "grin"
        after  = ddeAfter  </> name <.> "grin"
        specFun after' (lvaResult, cbyResult, transformed) = do
          expected <- runIO $ readFile after'
          let expected' = parseProg expected
          it name $ transformed `sameAs` expected'
