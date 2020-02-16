{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.ExtendedSyntax.Optimising.InterproceduralDeadFunctionEliminationSpec where

import Transformations.ExtendedSyntax.Optimising.InterproceduralDeadFunctionElimination (interproceduralDeadFunctionElimination)

import Data.Either (fromRight)

import Test.Hspec

import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.PrimOpsPrelude (withPrimPrelude)
import Grin.ExtendedSyntax.TypeCheck (inferTypeEnv)
import AbstractInterpretation.ExtendedSyntax.LiveVariableSpec (calcLiveness)


runTests :: IO ()
runTests = hspec spec

dfe :: Exp -> Exp
dfe e = either error id $
  interproceduralDeadFunctionElimination (calcLiveness e) (inferTypeEnv e) e

spec :: Spec
spec = do
  describe "Dead Function Elimination" $ do

    it "app_side_effect_1" $ do
      let before = [prog|
            grinMain =
              k0 <- pure 0
              n0 <- pure (CInt k0)
              p0 <- store n0
              y0 <- f p0
              y1 <- fetch p0
              pure y1

            f p =
              k1 <- pure 1
              n1 <- pure (CInt k1)
              _1 <- update p n1
              pure 0
          |]

      let after = [prog|
            grinMain =
              k0 <- pure 0
              n0 <- pure (CInt k0)
              p0 <- store n0
              y0 <- f p0
              y1 <- fetch p0
              pure y1

            f p =
              k1 <- pure 1
              n1 <- pure (CInt k1)
              _1 <- update p n1
              pure 0
          |]
      dfe before `sameAs` after

    it "mutually_recursive" $ do
      let before = [prog|
            grinMain = pure 0
            f x = g x
            g y = f y
          |]

      let after = [prog|
            grinMain = pure 0
          |]
      dfe before `sameAs` after

    it "replace_node" $ do
      let before = [prog|
            grinMain =
              k0 <- pure 0
              n0 <- f k0
              pure 0

            f x =
              k1 <- pure 1
              n1 <- pure (CInt k1)
              p <- store n1
              pure (CNode p)
          |]

      let after = [prog|
            grinMain =
              k0 <- pure 0
              n0 <- pure (#undefined :: {CNode[#ptr]})
              pure 0
          |]
      dfe before `sameAs` after

    it "replace_simple_type" $ do
      let before = [prog|
            grinMain =
              k0 <- pure 0
              y0 <- f k0
              pure 0

            f x = pure x
          |]

      let after = [prog|
            grinMain =
              k0 <- pure 0
              y0 <- pure (#undefined :: T_Int64)
              pure 0
          |]
      dfe before `sameAs` after

    it "simple" $ do
      let before = [prog|
            grinMain = pure 0

            f x = pure x
          |]

      let after = [prog|
            grinMain = pure 0
          |]
      dfe before `sameAs` after

    it "true_side_effect_min" $ do
      let before = withPrimPrelude [prog|
            grinMain =
              result_main <- Main.main1 $
              pure ()

            Main.main1 =
              k0 <- pure 0
              _prim_int_print $ k0
          |]

      let after = withPrimPrelude [prog|
            grinMain =
              result_main <- Main.main1 $
              pure ()

            Main.main1 =
              k0 <- pure 0
              _prim_int_print $ k0
          |]
      dfe before `sameAs` after
