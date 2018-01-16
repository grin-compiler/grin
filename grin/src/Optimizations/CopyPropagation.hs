{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor, OverloadedStrings #-}
module Optimizations.CopyPropagation where

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
Exactly what happens when an explicit node is involved in a copy depends on the following:
* if the copy is a left or a right copy
* if the left (or right) hand hide of the copy is a node variable or an explicit node
* if the lhs (or rhs) is an explicit node, it also depends on if the node tag
  is a known tag or a variable
-}

copyPropagationLeft :: Exp -> Exp
copyPropagationLeft = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    -- TODO: Handle all the cases with explicit nodes.
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

    it "the program size shrinks" $ property $ forAll nonWellFormedPrograms $ \original ->
      let transformed = copyPropagationLeft original
      in conjoin
          [ original `reducedSize` transformed
          , checkUniqueNames transformed
          ]

-- Check if the number of nodes in a program is less rhan or equals after the transformation.
reducedSize :: Exp -> Exp -> Property
reducedSize original transformed =
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

cpRunTests :: IO ()
cpRunTests = hspec Optimizations.CopyPropagation.tests
