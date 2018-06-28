{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.TrivialCaseEliminationSpec where

import Transformations.Optimising.TrivialCaseElimination

import Test.Hspec
import Grin.TH
import Test.Test hiding (newVar)
import Test.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "Figure 4.24" $ do
      let before = [expr|
          case v of
            (Ffun a1 a2 a3) -> fun a1 a2 a3
        |]
      let after = [expr|
          do
            (Ffun a1 a2 a3) <- pure v
            fun a1 a2 a3
        |]
      trivialCaseElimination (ctx before) `sameAs` (ctx after)

    it "bypass" $ do
      let before = [expr|
          case v of
            (Ffun1 a1 a2 a3) -> fun1 a1 a2 a3
            #default -> pure 2
        |]
      let after = [expr|
          case v of
            (Ffun1 a1 a2 a3) -> fun1 a1 a2 a3
            #default -> pure 2
        |]
      trivialCaseElimination (ctx before) `sameAs` (ctx after)

    it "default alternative" $ do
      let before = [expr|
          case v of
            #default -> pure 2
        |]
      let after = [expr|
          do
            pure 2
        |]
      trivialCaseElimination (ctx before) `sameAs` (ctx after)
