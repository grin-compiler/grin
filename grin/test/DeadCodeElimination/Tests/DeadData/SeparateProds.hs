module DeadCodeElimination.Tests.DeadData.SeparateProds where

  import System.FilePath

  import Grin.Grin

  import Test.Hspec
  import Test.Assertions

  import DeadCodeElimination.Tests.DeadData.Util

  (separateProdsBefore, separateProdsAfter, separateProdsSpec) = mkDDETestCase "separate_prods"
