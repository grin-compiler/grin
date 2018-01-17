{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor, OverloadedStrings #-}
module Optimizations.ConstantFolding where

import Check
import Grin
import Free
import Test
import Test.Hspec
import Test.QuickCheck
import Transformations
import Transformations.Substitution
import Data.Functor.Foldable

import qualified Data.Map.Strict as Map

{-
Constant folding is not part of the official grin optimization pipeline.
This transformation is used for demonstrate and experiment with the
testing.
-}

constantFolding :: Exp -> Exp
constantFolding = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    EBind (SReturn v) (Var n) rest | isConstant v ->
      project $ substitution (Map.singleton n v) rest

    rest ->
      project rest

tests :: Spec
tests = do
  describe "constant folding" $ do
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
      constantFolding x `shouldBe` e

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
      constantFolding x `shouldBe` e

    it "unused variable" $ do
      x <- buildExpM $
        "x" <=: store 3 $
        "u" <=: unit 4  $
        unit 5
      e <- buildExpM $
        "x" <=: store 3 $
        unit 5
      constantFolding x `shouldBe` e

    it "only one statement" $ do
      x <- buildExpM $
        def "fun" ["a", "b"] $
          "x" <=: unit 3     $
          unit "x"
      e <- buildExpM $
        def "fun" ["a", "b"] $
          unit 3
      constantFolding x `shouldBe` e

    it "the program size shrinks" $ property $ forAll nonWellFormedPrograms $ \original ->
      let transformed = constantFolding original
      in conjoin
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

programSize :: Exp -> Int
programSize = cata $ \case
  ProgramF  ds    -> sum ds
  DefF      _ _ a -> 1 + a
  -- Exp
  EBindF    a _ b -> sum [1,a,b]
  ECaseF    _ as  -> sum (1:as)
  -- Simple Expr
  SAppF     _ _   -> 1
  SReturnF  _     -> 1
  SStoreF   _     -> 1
  SFetchIF  _ _   -> 1
  SUpdateF  _ _   -> 1
  SBlockF   a     -> 1 + a
  -- Alt
  AltF _ a        -> 1 + a

cfRunTests :: IO ()
cfRunTests = hspec Optimizations.ConstantFolding.tests
