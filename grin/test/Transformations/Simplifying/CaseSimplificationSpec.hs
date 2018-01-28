{-# LANGUAGE TypeApplications, OverloadedStrings, LambdaCase #-}
module Transformations.Simplifying.CaseSimplificationSpec where

import Data.Monoid hiding (Alt)
import Transformations.Simplifying.CaseSimplification
import Test.Hspec
import Test.QuickCheck hiding (generate)
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
      "l1" <=: store @Int 3       $
      switch ("t" #: ["a1", "a2"])
        [ ("Nil"  @: [],
              unit @Int 3)
        , ("Cons" @: ["x", "xs"],
              Unit <=: store @Var "x"  $
              Unit <=: store @Var "xs" $
              unit @Int 5)
        ]

    after <- buildExpM $
      "l1" <=: store @Int 3 $
      switch "t"
        [ (tag "Nil"  0,
              unit @Int 3)
        , (tag "Cons" 2,
              Unit <=: store @Var "a1" $
              Unit <=: store @Var "a2" $
              unit @Int 5)
        ]

    caseSimplification before `shouldBe` after

  it "Program size does not change" $ property $
    forAll nonWellFormedPrograms programSizeDoesNotChange

  it "Cases with tags as values have tags in their alternatives" $ property $
    forAll nonWellFormedPrograms effectedAlternativesHasOnlyTags

varTagCover :: Exp -> Property -> Property
varTagCover exp =
  within 10000000 {-microsecond-} .
  cover (getAny $ valuesInCases (Any . isVarTagNode) exp) 1 "Case with VarTagNode"

programSizeDoesNotChange :: Exp -> Property
programSizeDoesNotChange exp = varTagCover exp $ unchangedSize exp $ caseSimplification exp

effectedAlternativesHasOnlyTags :: Exp -> Property
effectedAlternativesHasOnlyTags exp = varTagCover exp $ checkVarTagCases $ caseSimplification exp


isVarTagNode :: Val -> Bool
isVarTagNode = \case
  VarTagNode _ _ -> True
  _              -> False

unchangedSize :: Exp -> Exp -> Property
unchangedSize before after = property $ programSize before == programSize after

checkVarTagCases :: Exp -> Property
checkVarTagCases = \case
  ECase       val alts | isVarTagNode val -> mconcat (checkAlt <$> alts)

  Program     defs -> mconcat (checkVarTagCases <$> defs)
  Def         name params body -> checkVarTagCases body

  EBind       se lpat exp -> checkVarTagCases se <> checkVarTagCases exp
  ECase       val alts -> mconcat (checkVarTagCases <$> alts)

  SBlock      exp -> checkVarTagCases exp
  Alt cpat exp -> checkVarTagCases exp

  rest -> property True
  where
    checkAlt :: Exp -> Property
    checkAlt (Alt cpat exp) = checkVarTagCases exp <> property (isBasicCPat cpat)


instance Monoid Property where
  mempty      = property True
  mappend p q = p .&&. q
  mconcat ps  = conjoin ps
