{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor, OverloadedStrings #-}
module Optimizations.CopyPropagation where

import Grin
import Free
import Test.Hspec
import Transformations
import Transformations.Substitution
import Data.Functor.Foldable

import qualified Data.Map.Strict as Map

copyPropagationLeft :: Exp -> Exp
copyPropagationLeft = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    EBind (SReturn v) (Var n) rest ->
      project $ substitution (Map.singleton n v) rest

    rest ->
      project rest

tests :: Spec
tests = do
  describe "copy propagation left" $ do
    it "inside bind" $ do
      x <- buildExpM $
        "x"  <=: store "a" $
        "y"  <=: store "b" $
        "u"  <=: unit 5    $
        Unit <=: store "u" $
        unit "u"

      e <- buildExpM $
        "x"  <=: store "a" $
        "y"  <=: store "b" $
        Unit <=: store 5   $
        unit 5
      copyPropagationLeft x `shouldBe` e

    it "last bind" $ do
      x <- buildExpM $
        "x" <=: store "a" $
        "y" <=: store "b" $
        "u" <=: unit 5    $
        unit "u"
      e <- buildExpM $
        "x" <=: store "a" $
        "y" <=: store "b" $
        unit 5
      copyPropagationLeft x `shouldBe` e

    it "unused variable" $ do
      x <- buildExpM $
        "x" <=: store 3 $
        "u" <=: unit 4  $
        unit 5
      e <- buildExpM $
        "x" <=: store 3 $
        unit 5
      copyPropagationLeft x `shouldBe` e

    it "only one statement" $ do
      x <- buildExpM $
        def "fun" ["a", "b"] $
          "x" <=: unit 3     $
          unit "x"
      e <- buildExpM $
        def "fun" ["a", "b"] $
          unit 3
      copyPropagationLeft x `shouldBe` e

cpRunTests :: IO ()
cpRunTests = hspec Optimizations.CopyPropagation.tests
