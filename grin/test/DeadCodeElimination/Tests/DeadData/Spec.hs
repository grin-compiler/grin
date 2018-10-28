module DeadCodeElimination.Tests.DeadData.Spec where

import System.FilePath

import Test.IO
import Test.Hspec

import Grin.Grin
import Grin.TypeCheck (typeEnvFromHPTResult)

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
import DeadCodeElimination.Tests.DeadData.FNode
import DeadCodeElimination.Tests.DeadData.PNode
import DeadCodeElimination.Tests.DeadData.PNodeOpt

import DeadCodeElimination.Tests.DeadData.ProducerGrouping


spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

producerGroupingTestName :: String 
producerGroupingTestName = "Producer Grouping"

ddeTestName :: String 
ddeTestName = "Dead Data Elimination"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = do
  testGroup producerGroupingTestName $ do
    mkSpecFromWith fromCurDir calcProducerGraphAll    [multiProdSimpleSrc] [multiProdSimpleAllSpec]
    mkSpecFromWith fromCurDir calcProducerGraphActive [multiProdSimpleSrc] [multiProdSimpleActiveSpec]
  
  testGroup ddeTestName $
    mkBeforeAfterSpecFrom fromCurDir eliminateDeadData
      [ impossibleAltBefore
      , lengthBefore
      , multipleFieldsBefore
      , onlyDummifyBefore
      , deletableSingleBefore
      , deletableMultiBefore
      , separateProdsBefore
      , fNodeBefore
      , pNodeBefore
      , pNodeOptBefore
      ]
      [ impossibleAltAfter
      , lengthAfter
      , multipleFieldsAfter
      , onlyDummifyAfter
      , deletableSingleAfter
      , deletableMultiAfter
      , separateProdsAfter
      , fNodeAfter
      , pNodeAfter
      , pNodeOptAfter
      ]
      [ impossibleAltSpec
      , lengthSpec
      , multipleFieldsSpec
      , onlyDummifySpec
      , deletableSingleSpec
      , deletableMultiSpec
      , separateProdsSpec
      , fNodeSpec
      , pNodeSpec
      , pNodeOptSpec
      ]

calcProducerGraphAll :: Exp -> ProducerGraph
calcProducerGraphAll = groupAllProducers 
                    . _producers 
                    . calcCByResult

calcProducerGraphActive :: Exp -> ProducerGraph
calcProducerGraphActive 
  = uncurry groupActiveProducers
  . ((,) <$> calcLiveness <*> (_producers . calcCByResult))

eliminateDeadData :: Exp -> Exp
eliminateDeadData e = e'
  where lvaResult = calcLiveness e
        cbyResult = calcCByResult e
        Right env = typeEnvFromHPTResult (_hptResult cbyResult)
        Right e'  = deadDataElimination lvaResult cbyResult env e
