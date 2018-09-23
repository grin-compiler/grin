module CreatedBy.CreatedBySpec where

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Set as S

import System.FilePath

import Grin.Grin

import Test.IO
import Test.Test
import Test.Hspec
import Test.Assertions

import AbstractInterpretation.IR hiding (Tag)
import AbstractInterpretation.Reduce
import AbstractInterpretation.CreatedBy
import AbstractInterpretation.CByResult


spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = runTestsFromWith fromCurDir calcProducers
  [ puresSrc
  , funCallSrc
  , caseSimpleSrc
  , heapSrc
  , pointerInNodeSrc
  , caseRestricted1Src
  , caseRestricted2Src
  , caseRestricted3Src
  ]
  [ puresSpec
  , funCallSpec
  , caseSimpleSpec
  , heapSpec
  , pointerInNodeSpec
  , caseRestricted1Spec
  , caseRestricted2Spec
  , caseRestricted3Spec
  ]

cbyExamples :: FilePath
cbyExamples = "CreatedBy" </> "examples"

calcCByResult :: Exp -> CByResult
calcCByResult prog
  | Right cbyProgram <- codeGen prog
  , computer <- evalDataFlowInfo cbyProgram
  , cbyResult <- toCByResult cbyProgram computer
  = cbyResult

calcProducers :: Exp -> ProducerMap
calcProducers = _producers . calcCByResult

mkProducerSet :: [(Tag, [Name])] -> ProducerSet
mkProducerSet = ProducerSet . M.fromList . map (\(t,xs) -> (t,S.fromList xs))

emptyProducerSet :: ProducerSet
emptyProducerSet = mkProducerSet []

restrictedBy :: ProducerSet -> Tag -> ProducerSet
restrictedBy (ProducerSet ps) tag = ProducerSet $ M.filterWithKey (\k _ -> k == tag) ps



puresSrc :: FilePath
puresSrc = cbyExamples </> "pures.grin"

puresExpected :: ProducerMap
puresExpected = ProducerMap $
  M.fromList [ ("a", producerA)
             , ("b", producerA)
             , ("c", producerA)
             ]
  where producerA = mkProducerSet [(Tag C "Int", ["a"])]

puresSpec :: ProducerMap -> Spec
puresSpec found = it "pures" $ found `sameAs` puresExpected



funCallSrc :: FilePath
funCallSrc = cbyExamples </> "function_call.grin"

funCallExpected :: ProducerMap
funCallExpected = ProducerMap $
  M.fromList [ ("a",  producerA)
             , ("b",  producerA)
             , ("c",  producerX1)
             , ("d",  producerX1)
             , ("x",  emptyProducerSet)
             , ("x1", producerX1)
             , ("y",  emptyProducerSet)
             , ("y1", producerX1)
             ]
  where producerA  = mkProducerSet [(Tag C "Int", ["a"])]
        producerX1 = mkProducerSet [(Tag C "Int", ["x1"])]

funCallSpec :: ProducerMap -> Spec
funCallSpec found = it "function_call" $ found `sameAs` funCallExpected



caseSimpleSrc :: FilePath
caseSimpleSrc = cbyExamples </> "case_simple.grin"

caseSimpleExpected :: ProducerMap
caseSimpleExpected = ProducerMap $
  M.fromList [ ("a",  producerA)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             ]
  where producerA  = mkProducerSet [ (Tag C "Int",  ["x0"])
                                   , (Tag C "Bool", ["x1"])
                                   ]
        producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
        producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]

caseSimpleSpec :: ProducerMap -> Spec
caseSimpleSpec found = it "case_simple" $ found `sameAs` caseSimpleExpected



heapSrc :: FilePath
heapSrc = cbyExamples </> "heap.grin"

heapExpected :: ProducerMap
heapExpected = ProducerMap $
  M.fromList [ ("x0", producerX0)
             , ("x1", producerX1)
             , ("x2", producerX2)
             , ("p0", emptyProducerSet)
             , ("p1", emptyProducerSet)
             , ("y0", producerY0)
             , ("y1", producerY1)
             ]
  where producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
        producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
        producerX2 = mkProducerSet [(Tag C "Bool", ["x2"])]
        producerY0 = producerX0 <> producerX2
        producerY1 = producerX1 <> producerX2

heapSpec :: ProducerMap -> Spec
heapSpec found = it "heap" $ found `sameAs` heapExpected



caseRestricted1Src :: FilePath
caseRestricted1Src = cbyExamples </> "case_restricted_1.grin"

caseRestricted1Expected :: ProducerMap
caseRestricted1Expected = ProducerMap $
  M.fromList [ ("a0", producerA0)
             , ("r0", producerR0)
             , ("b0", producerB0)
             , ("b1", producerB1)
             , ("b2", emptyProducerSet)
             , ("c0", emptyProducerSet)
             , ("c1", emptyProducerSet)
             , ("c2", emptyProducerSet)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             ]
  where producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
        producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
        producerA0 = producerX0 <> producerX1
        producerB0 = mkProducerSet [(Tag C "Int",  ["b0"])]
        producerB1 = mkProducerSet [(Tag C "Bool", ["b1"])]
        producerR0 = producerB0 <> producerB1

caseRestricted1Spec :: ProducerMap -> Spec
caseRestricted1Spec found = it "case_restricted_1" $ found `sameAs` caseRestricted1Expected



caseRestricted2Src :: FilePath
caseRestricted2Src = cbyExamples </> "case_restricted_2.grin"

caseRestricted2Expected :: ProducerMap
caseRestricted2Expected = ProducerMap $
  M.fromList [ ("a0", producerA0)
             , ("r0", producerR0)
             , ("b0", producerB0)
             , ("b1", producerB1)
             , ("b2", emptyProducerSet)
             , ("c0", emptyProducerSet)
             , ("c1", emptyProducerSet)
             , ("c2", emptyProducerSet)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             ]
  where producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
        producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
        producerA0 = producerX0 <> producerX1
        producerB0 = producerX0 <> producerX1
        producerB1 = mkProducerSet [(Tag C "Bool", ["b1"])]
        producerR0 = producerB0 <> producerB1

caseRestricted2Spec :: ProducerMap -> Spec
caseRestricted2Spec found = it "case_restricted_2" $ found `sameAs` caseRestricted2Expected



caseRestricted3Src :: FilePath
caseRestricted3Src = cbyExamples </> "case_restricted_3.grin"

caseRestricted3Expected :: ProducerMap
caseRestricted3Expected = ProducerMap $
  M.fromList [ ("a0", producerA0)
             , ("a1", producerA1)
             , ("r0", producerR0)
             , ("b0", producerB0)
             , ("b1", producerB1)
             , ("b2", emptyProducerSet)
             , ("c0", emptyProducerSet)
             , ("c1", emptyProducerSet)
             , ("c2", emptyProducerSet)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             , ("y",  producerY)
             , ("y0", producerY0)
             , ("y1", emptyProducerSet) -- because the control never reaches it
             , ("y2", producerY2)
             , ("n",  emptyProducerSet)
             , ("b",  emptyProducerSet)
             , ("w",  emptyProducerSet)
             ]
  where producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
        producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
        producerA0 = producerX0 <> producerX1
        producerA1 = mkProducerSet [(Tag C "Word", ["a1"])]
        producerY  = producerA0 `restrictedBy` (Tag C "Int") <> producerA1
        producerY0 = mkProducerSet [(Tag C "Int",  ["y0"])]
        producerY1 = mkProducerSet [(Tag C "Bool", ["y1"])]
        producerY2 = mkProducerSet [(Tag C "Word", ["y2"])]
        producerB0 = producerY0 <> producerY2 -- because the analysis is not context sensitive
        producerB1 = producerY0 <> producerY2 -- because the analysis is not context sensitive
        producerR0 = producerB0 <> producerB1

caseRestricted3Spec :: ProducerMap -> Spec
caseRestricted3Spec found = it "case_restricted_3" $ found `sameAs` caseRestricted3Expected



pointerInNodeSrc :: FilePath
pointerInNodeSrc = cbyExamples </> "pointer_in_node.grin"

pointerInNodeExpected :: ProducerMap
pointerInNodeExpected = ProducerMap $
  M.fromList [ ("n0",  producerN0)
             , ("p0",  emptyProducerSet)
             , ("n1",  producerN1)
             , ("x",   emptyProducerSet)
             , ("pxs", emptyProducerSet)
             , ("xs",  producerXS)
             ]
  where producerN0 = mkProducerSet [(Tag C "Nil",  ["n0"])]
        producerN1 = mkProducerSet [(Tag C "Cons", ["n1"])]
        producerXS = producerN0

pointerInNodeSpec :: ProducerMap -> Spec
pointerInNodeSpec found = it "pointer_in_node" $ found `sameAs` pointerInNodeExpected
