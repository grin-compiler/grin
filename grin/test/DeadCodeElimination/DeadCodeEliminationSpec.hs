module DeadCodeElimination.DeadCodeEliminationSpec where

import System.FilePath

import Test.IO
import Test.Hspec

import Grin.Grin

-- TODO: delete these
import Grin.Parse
import Grin.Pretty

import AbstractInterpretation.CByUtil
import AbstractInterpretation.CByResult (CByResult)
import AbstractInterpretation.LVAResult (LVAResult)

import Transformations.Optimising.DeadDataElimination

import CreatedBy.CreatedBySpec (calcCByResult)
import LiveVariable.LiveVariableSpec (calcLiveness)

import DeadCodeElimination.Tests.DeadData.MultipleFields
import DeadCodeElimination.Tests.DeadData.OnlyDummify
import DeadCodeElimination.Tests.ProducerGrouping


-- TODO: delete
prettyActiveProducerGraphIO :: FilePath -> IO ()
prettyActiveProducerGraphIO fp = do
  src <- readFile fp
  let prog = parseProg src
  print . pretty
        . _producerGraph
        . groupActiveProducers (calcLiveness prog) (calcCByResult prog)
        $ prog

printAllProducersIO :: FilePath -> IO ()
printAllProducersIO fp = do
  src <- readFile fp
  let prog = parseProg src
  print . collectProducers
        $ prog

printActiveProducersIO :: FilePath -> IO ()
printActiveProducersIO fp = do
  src <- readFile fp
  let prog = parseProg src
  print . collectActiveProducers (calcLiveness prog)
        $ prog



spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = do
  runTestsFromWith fromCurDir calcProducerGraph [multiProdSimpleSrc] [multiProdSimpleSpec]
  runTestsFromWith fromCurDir eliminateDeadData
    [ multipleFieldsSrc
    , onlyDummifySrc
    ]
    [ multipleFieldsSpec
    , onlyDummifySpec
    ]

calcProducerGraph :: Exp -> ProducerGraph
calcProducerGraph e = groupAllProducers (calcCByResult e)

eliminateDeadData :: Exp -> (LVAResult, CByResult, Exp)
eliminateDeadData e = (lvaResult, cbyResult, transformed)
  where lvaResult   = calcLiveness e
        cbyResult   = calcCByResult e
        transformed = deadDataElimination lvaResult cbyResult e
