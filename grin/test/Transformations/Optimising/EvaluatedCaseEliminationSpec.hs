{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.EvaluatedCaseEliminationSpec where

import Transformations.Optimising.EvaluatedCaseElimination

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "Figure 4.22" $ do
      let before = [expr|
          case v of
            (CLeft l)  -> pure v
            (CRight r) -> pure v
        |]
      let after = [expr|
          pure v
        |]
      evaluatedCaseElimination (ctx before) `sameAs` (ctx after)

    it "default case" $ do
      let before = [expr|
          case v of
            (CLeft l)   -> pure v
            #default    -> pure v
        |]
      let after = [expr|
          pure v
        |]
      evaluatedCaseElimination (ctx before) `sameAs` (ctx after)
