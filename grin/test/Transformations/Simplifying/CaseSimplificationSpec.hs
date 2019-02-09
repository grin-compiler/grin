{-# LANGUAGE OverloadedStrings, LambdaCase, QuasiQuotes #-}
module Transformations.Simplifying.CaseSimplificationSpec where

import Control.Monad
import Data.Monoid hiding (Alt)
import Transformations.Simplifying.CaseSimplification
import Test.Hspec
import Test.QuickCheck hiding (generate)
import Test.Test

import Test.Check
import Grin.Grin
import Grin.TH
import Test.Assertions


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
  testExprContextE $ \ctx -> do
    it "Example from Figure 4.11" $ do
      let before =
            [expr|
              l1 <- store (CNone)
              case (t a1 a2) of
                CNil -> pure 3
                (CCons x xs) -> store x
                                store xs
                                pure 5
            |]
      let after =
            [expr|
              l1 <- store (CNone)
              case t of
                CNil -> pure 3
                CCons -> store a1
                         store a2
                         pure 5
            |]
      pending
      caseSimplification (ctx before) `sameAs` (ctx after)

  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "Program size does not change" $ do
        pending
    -- NOTE: commented out due type error
    {-
        property $ forAll gen programSizeDoesNotChange
    -}
      it "Cases with tas as values have tags in their alternatives" $ do
        pending
    -- NOTE: commented out due type error
    {-
        property $ forAll gen effectedAlternativesHasOnlyTags
    -}

varTagCover :: Exp -> Property -> Property
varTagCover exp =
  within 10000000 {-microsecond-} .
  cover 1 (getAny $ valuesInCases (Any . isVarTagNode) exp) "Case with VarTagNode"

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

  Program _   defs -> mconcat (checkVarTagCases <$> defs)
  Def         name params body -> checkVarTagCases body

  EBind       se lpat exp -> checkVarTagCases se <> checkVarTagCases exp
  ECase       val alts -> mconcat (checkVarTagCases <$> alts)

  SBlock      exp -> checkVarTagCases exp
  Alt cpat exp -> checkVarTagCases exp

  rest -> property True
  where
    checkAlt :: Exp -> Property
    checkAlt (Alt cpat exp) = checkVarTagCases exp <> property (isBasicCPat cpat)


instance Semigroup Property where
  p <> q = p .&&. q

instance Monoid Property where
  mempty      = property True
  mconcat ps  = conjoin ps
