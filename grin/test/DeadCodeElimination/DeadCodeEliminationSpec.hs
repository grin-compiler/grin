module DeadCodeElimination.DeadCodeEliminationSpec where

import System.FilePath

import Test.IO
import Test.Hspec

import Grin.Grin

import AbstractInterpretation.CByUtil
import AbstractInterpretation.CByResult (CByResult(..))
import AbstractInterpretation.LVAResult (LVAResult)

import Transformations.Optimising.DeadDataElimination

import CreatedBy.CreatedBySpec (calcCByResult)
import LiveVariable.LiveVariableSpec (calcLiveness)

import DeadCodeElimination.Tests.DeadData.ImpossibleAlt
import DeadCodeElimination.Tests.DeadData.Length
import DeadCodeElimination.Tests.DeadData.MultipleFields
import DeadCodeElimination.Tests.DeadData.OnlyDummify
import DeadCodeElimination.Tests.DeadData.DeletableSingle
import DeadCodeElimination.Tests.DeadData.DeletableMulti
import DeadCodeElimination.Tests.DeadData.SeparateProds
import DeadCodeElimination.Tests.ProducerGrouping


spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = do
  runTestsFromWith fromCurDir calcProducerGraphAll    [multiProdSimpleSrc] [multiProdSimpleAllSpec]
  runTestsFromWith fromCurDir calcProducerGraphActive [multiProdSimpleSrc] [multiProdSimpleActiveSpec]
  runBeforeAfterTestsFromWith fromCurDir eliminateDeadData
    [ impossibleAltBefore
    , lengthBefore
    , multipleFieldsBefore
    , onlyDummifyBefore
    , deletableSingleBefore
    , deletableMultiBefore
    , separateProdsBefore
    ]
    [ impossibleAltAfter
    , lengthAfter
    , multipleFieldsAfter
    , onlyDummifyAfter
    , deletableSingleAfter
    , deletableMultiAfter
    , separateProdsAfter
    ]
    [ impossibleAltSpec
    , lengthSpec
    , multipleFieldsSpec
    , onlyDummifySpec
    , deletableSingleSpec
    , deletableMultiSpec
    , separateProdsSpec
    ]

calcProducerGraphAll :: Exp -> ProducerGraph
calcProducerGraphAll = groupAllProducers 
                    . _producers 
                    . calcCByResult

calcProducerGraphActive :: Exp -> ProducerGraph
calcProducerGraphActive 
  = uncurry groupActiveProducers
  . ((,) <$> calcLiveness <*> (_producers . calcCByResult))

eliminateDeadData :: Exp -> (LVAResult, CByResult, Exp)
eliminateDeadData e = (lvaResult, cbyResult, e')
  where lvaResult = calcLiveness e
        cbyResult = calcCByResult e
        Right e'  = deadDataElimination lvaResult cbyResult e
