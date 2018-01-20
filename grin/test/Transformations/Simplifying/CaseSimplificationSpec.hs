{-# LANGUAGE TypeApplications, OverloadedStrings, LambdaCase #-}
module Transformations.Simplifying.CaseSimplificationSpec where

import Data.Monoid
import Transformations.Simplifying.CaseSimplification
import Test.Hspec
import Test.QuickCheck
import Test

import Check
import Free
import Grin

runTests :: IO ()
runTests = hspec spec

{-
Before the case simplificaiton the scrutinised value in case expressions are
normally node variables, which the vectorization changes to explicit node values.
-}

{-
After the case simplification, all case expressions will be scrutinising only
basic values (including tag values), and all case patterns will correspondingly
be just basic values. The patterns will not contain (and bind) any variables.
-}

spec :: Spec
spec = do
  it "Example from Figure 4.11" $ do
    before <- buildExpM $
      Unit <=: store @Int 3       $
      switch ("t" #: ["a1", "a2"])
        [ ("Nil"  @: [],
              unit @Int 3)
        , ("Cons" @: ["x", "xs"],
              Unit <=: store @Var "x"  $
              Unit <=: store @Var "xs" $
              unit @Int 5)
        ]

    after <- buildExpM $
      Unit <=: store @Int 3 $
      switch "t"
        [ (tag "Nil"  0,
              unit @Int 3)
        , (tag "Cons" 2,
              Unit <=: store @Var "a1" $
              Unit <=: store @Var "a2" $
              unit @Int 5)
        ]

    caseSimplification before `shouldBe` after

  it "Program size does not change" $ property $ forAll nonWellFormedPrograms $ \before ->
    let after = caseSimplification before
        sizeBefore = programSize before
        sizeAfter  = programSize after
        isVarTagNode = \case
          VarTagNode _ _ -> Any True
          _              -> Any False
    in cover (getAny $ valuesInCases isVarTagNode before) 1 "Case with VarTagNode"
       $ sizeBefore == sizeAfter
