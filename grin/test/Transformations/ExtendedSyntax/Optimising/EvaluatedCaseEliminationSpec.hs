{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.EvaluatedCaseEliminationSpec where

import Transformations.ExtendedSyntax.Optimising.EvaluatedCaseElimination

import Test.Hspec
import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.New.Test hiding (newVar)
import Test.ExtendedSyntax.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "Figure 4.22" $ do
      let before = [expr|
          case v of
            (CLeft l)  @ alt1 -> pure v
            (CRight r) @ alt2 -> pure v
        |]
      let after = [expr|
          pure v
        |]
      evaluatedCaseElimination (ctx before) `sameAs` (ctx after)

    it "default case" $ do
      let before = [expr|
          case v of
            (CLeft l) @ alt1 -> pure v
            #default  @ alt2 -> pure v
        |]
      let after = [expr|
          pure v
        |]
      evaluatedCaseElimination (ctx before) `sameAs` (ctx after)
