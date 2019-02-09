{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}
module Transformations.Optimising.ConstantFoldingSpec where

import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Transformations.Optimising.ConstantFolding
import Test.Check
import Grin.Grin
import Grin.TH
import Test.Test
import Test.Assertions


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
              u <- pure 5
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
              u <- pure 4
              pure 5
            |]
      constantFolding before `sameAs` after

    it "node constant" $ do
      let before =
            [expr|
              i <- pure 0
              f <- pure 1.1
              a <- pure (CPair i f)
              store a
            |]
      let after =
            [expr|
              i <- pure 0
              f <- pure 1.1
              a <- pure (CPair 0 1.1)
              store (CPair 0 1.1)
            |]
      constantFolding before `sameAs` after

    it "node pattern" $ do
      let before =
            [expr|
              i <- pure 0
              f <- pure 1.1
              a <- pure (CPair i f)
              (CPair j g) <- pure a
              store (CPair j g)
            |]
      let after =
            [expr|
              i <- pure 0
              f <- pure 1.1
              a <- pure (CPair 0 1.1)
              (CPair j g) <- pure (CPair 0 1.1)
              store (CPair 0 1.1)
            |]
      constantFolding before `sameAs` after

    it "node pattern mismatch" $ do
      let before =
            [expr|
              a <- pure (CPair 0 1.1)
              (CTriple k l m) <- pure a
              store (CTriple k l m)
            |]
      let after =
            [expr|
              a <- pure (CPair 0 1.1)
              (CTriple k l m) <- pure (CPair 0 1.1)
              store (CTriple k l m)
            |]
      constantFolding before `sameAs` after

    it "bugfix - cascade" $ do
      let before =
            [expr|
              i <- pure 0
              f <- pure 1.1
              (CNil) <- pure (CNil)
              store (CPair i f)
            |]
      let after =
            [expr|
              i <- pure 0
              f <- pure 1.1
              (CNil) <- pure (CNil)
              store (CPair 0 1.1)
            |]
      constantFolding before `sameAs` after


  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "the program size shrinks" $ pending {- property $ forAll gen $ \original ->
        let transformed = constantFolding original
        in changed original transformed $ conjoin
            [ transformed `smallerThan` original
            , checkUniqueNames transformed
            ]
        -}

-- Check if the number of nodes in a program is less rhan or equals after the transformation.
smallerThan :: Exp -> Exp -> Property
smallerThan transformed original  =
  let sizeReduced  = programSize transformed
      sizeOriginal = programSize original
  in
    cover 0 (sizeReduced == sizeOriginal) "Non Reduced" $
    cover 1 (sizeReduced <  sizeOriginal) "Reduced"     $
    (sizeReduced <= sizeOriginal)

checkUniqueNames :: Exp -> Property
checkUniqueNames = label "Unique name" . null . nonUniqueNames

runTests :: IO ()
runTests = hspec spec
