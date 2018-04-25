{-# LANGUAGE OverloadedStrings, LambdaCase, QuasiQuotes #-}
module Transformations.Simplifying.VectorisationSpec where

import AbstractInterpretation.AbstractRunGrin
import AbstractInterpretation.HPTResult
import Transformations.Simplifying.Vectorisation (vectorisation)

import Data.Monoid
import Data.Map
import Test.Hspec
import Grin
import GrinTH
import Assertions
import Test

import qualified Data.Map as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "Example from Figure 4.9" $ do
      let hpt = Computer
                  mempty
                  (Map.fromList
                    [ ("v", (Set.singleton
                              (N (RTNode (Tag C "Cons")
                                  [ Set.singleton (BAS T_Int64)
                                  , Set.singleton (RTLoc 3)
                                  ]))))
                    , ("l0", (Set.singleton
                               (V (BAS T_Int64))))
                    , ("l1", (Set.singleton
                               (V (BAS T_Int64))))
                    ])
                  mempty

      let before = [expr|
              l0 <- store (CNone)
              v  <- pure (Cq p1 p2)
              l1 <- store v
              pure 1
            |]
      let after = [expr|
              l0         <- store (CNone)
              (v0 v1 v2) <- pure (Cq p1 p2)
              l1         <- store (v0 v1 v2)
              pure 1
            |]
      pending
      vectorisation hpt (ctx before) `sameAs` (ctx after)

runTests :: IO ()
runTests = hspec spec
