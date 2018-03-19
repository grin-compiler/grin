{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}
module Transformations.Optimising.ConstantFoldingSpec where

import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Transformations.Optimising.ConstantFolding
import Check
import Grin
import GrinTH
import Test
import Assertions


spec :: Spec
spec = do
  describe "constant folding" $ do
    it "last bind" $ do
      let before =
            [expr|
              x <- store a
              y <- store b
              u <- pure 5
              pure u
            |]
      let after =
            [expr|
              x <- store a
              y <- store b
              pure 5
            |]
      constantFolding before `sameAs` after

    it "unused variable" $ do
      let before =
            [expr|
              x <- store (CNone)
              u <- pure 4
              pure 5
            |]
      let after =
            [expr|
              x <- store (CNone)
              pure 5
            |]
      constantFolding before `sameAs` after

  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "the program size shrinks" $ property $ forAll gen $ \original ->
        let transformed = constantFolding original
        in changed original transformed $ conjoin
            [ transformed `smallerThan` original
            , checkUniqueNames transformed
            ]

-- Check if the number of nodes in a program is less rhan or equals after the transformation.
smallerThan :: Exp -> Exp -> Property
smallerThan transformed original  =
  let sizeReduced  = programSize transformed
      sizeOriginal = programSize original
  in
    cover (sizeReduced == sizeOriginal) 0 "Non Reduced" $
    cover (sizeReduced <  sizeOriginal) 1 "Reduced"     $
    (sizeReduced <= sizeOriginal)

checkUniqueNames :: Exp -> Property
checkUniqueNames = label "Unique name" . null . nonUniqueNames

runTests :: IO ()
runTests = hspec spec
