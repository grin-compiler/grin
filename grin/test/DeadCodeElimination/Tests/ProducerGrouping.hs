module DeadCodeElimination.Tests.ProducerGrouping where

import System.FilePath

import Grin.Grin

import Test.Test
import Test.Hspec
import Test.Assertions

import AbstractInterpretation.CByUtil
import AbstractInterpretation.CreatedBy
import Transformations.Optimising.DeadDataElimination

import CreatedBy.CreatedBySpec (calcCByResult)
import DeadCodeElimination.Tests.Util


n0 = "n0"
n1 = "n1"
n2 = "n2"
n3 = "n3"


multiProdSimpleSrc :: FilePath
multiProdSimpleSrc = dceExamples </> "multi_prod_simple.grin"

multiProdSimpleExpected :: ProducerGraph
multiProdSimpleExpected = mkGraph
  [ (n0, [ (cInt, [n0])
         ]
    )
  , (n1, [ (cBool, [n1, n2, n3])
         ]
    )
  , (n2, [ (cBool, [n1, n2, n3])
         ]
    )
  , (n3, [ (cBool, [n1, n2, n3])
         ]
    )
  ]

multiProdSimpleSpec :: ProducerGraph -> Spec
multiProdSimpleSpec found = it "multi_prod_simple" $ found `sameAs` multiProdSimpleExpected
