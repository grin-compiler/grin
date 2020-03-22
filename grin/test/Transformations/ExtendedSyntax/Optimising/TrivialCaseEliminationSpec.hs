{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.TrivialCaseEliminationSpec where

import Transformations.ExtendedSyntax.Optimising.TrivialCaseElimination

import Test.Hspec
import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.New.Test hiding (newVar)
import Test.ExtendedSyntax.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "Figure 4.24" $ do
      let before = [expr|
          case v of
            (Ffun a1 a2 a3) @ alt1 -> fun a1 a2 a3
        |]
      let after = [expr|
          do
            (Ffun a1 a2 a3) @ alt1 <- pure v
            fun a1 a2 a3
        |]
      trivialCaseElimination (ctx before) `sameAs` (ctx after)

    it "bypass" $ do
      let before = [expr|
          case v of
            (Ffun1 a1 a2 a3) @ alt1 -> fun1 a1 a2 a3
            #default         @ alt2 -> pure 2
        |]
      let after = [expr|
          case v of
            (Ffun1 a1 a2 a3) @ alt1 -> fun1 a1 a2 a3
            #default         @ alt2 -> pure 2
        |]
      trivialCaseElimination (ctx before) `sameAs` (ctx after)

    it "default alternative" $ do
      let before = [expr|
          case v of
            #default @ alt1 -> pure 2
        |]
      let after = [expr|
          do
            alt1 <- pure v
            pure 2
        |]
      trivialCaseElimination (ctx before) `sameAs` (ctx after)
