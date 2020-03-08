{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.ExtendedSyntax.Optimising.InterproceduralDeadParameterEliminationSpec where

import Transformations.ExtendedSyntax.Optimising.InterproceduralDeadParameterElimination (interproceduralDeadParameterElimination)

import Data.Either

import Test.Hspec

import Test.ExtendedSyntax.Util (loadTestData)
import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.PrimOpsPrelude (withPrimPrelude)
import Grin.ExtendedSyntax.TypeCheck (inferTypeEnv)
import AbstractInterpretation.ExtendedSyntax.LiveVariableSpec (calcLiveness)


runTests :: IO ()
runTests = hspec spec

dpe :: Exp -> Exp
dpe e = either error id $
  interproceduralDeadParameterElimination (calcLiveness e) (inferTypeEnv e) e

spec :: Spec
spec = do
  describe "Dead Parameter Elimination" $ do

    it "Fnode" $ do
      let before = [prog|
            grinMain =
              k0 <- pure 5
              x0 <- pure (CInt k0)
              p0 <- store x0
              a0 <- pure (Ffoo p0 p0 p0)
              p1 <- store a0
              a1 <- eval p1
              pure a1

            -- functions cannot return pointers
            foo x y z =
              y' <- eval y
              pure y'

            eval p =
              v <- fetch p
              case v of
                (CInt n) @ alt1 -> pure v
                (Ffoo x1 y1 z1) @ alt2 ->
                  w <- foo x1 y1 z1
                  _1 <- update p w
                  pure w
          |]

      let after = [prog|
            grinMain =
              k0 <- pure 5
              x0 <- pure (CInt k0)
              p0 <- store x0
              a0 <- pure (Ffoo p0 p0 p0)
              p1 <- store a0
              a1 <- eval p1
              pure a1

            -- functions cannot return pointers
            foo y =
              z <- pure (#undefined :: #ptr)
              x <- pure (#undefined :: #ptr)
              y' <- eval y
              pure y'

            eval p =
              v <- fetch p
              case v of
                (CInt n) @ alt1 -> pure v
                (Ffoo x1 y1 z1) @ alt2 ->
                  w <- foo y1
                  _1 <- update p w
                  pure w
          |]
      dpe before `sameAs` after

    -- TODO: reenable
    -- it "Pnode" $ pipeline
    --   "dead-parameter-elimination/pnode_before.grin"
    --   "dead-parameter-elimination/pnode_after.grin"
    --   deadParameterEliminationPipeline


    it "PNode" $ do
      before <- loadTestData "dead-parameter-elimination/pnode.grin"
      after  <- loadTestData "dead-parameter-elimination/pnode.grin.expected"
      dpe before `sameAs` after

    it "Pnode opt" $ do
      let before = [prog|
            grinMain =
              k0 <- pure 5
              a0 <- pure (CInt k0)
              a1 <- pure (CInt k0)
              a2 <- pure (CInt k0)
              p0 <- store a0
              p1 <- store a1
              p2 <- store a2

              foo3 <- pure (P3foo)

              (P3foo) @ _1 <- pure foo3
              foo2 <- pure (P2foo p0)

              (P2foo v0) @ _2 <- pure foo2
              foo1 <- pure (P1foo v0 p1)

              (P1foo v1 v2) @ _3 <- pure foo1
              fooRet <- foo v1 v2 p2
              pure fooRet

            foo x0 y0 z0 =
              y0' <- fetch y0
              (CInt n) @ _4 <- y0'
              pure y0'
          |]

      let after = [prog|
            grinMain =
              k0 <- pure 5
              a0 <- pure (CInt k0)
              a1 <- pure (CInt k0)
              a2 <- pure (CInt k0)
              p0 <- store a0
              p1 <- store a1
              p2 <- store a2

              foo3 <- pure (P3foo)

              (P3foo) @ _1 <- pure foo3
              foo2 <- pure (P2foo p0)

              (P2foo v0) @ _2 <- pure foo2
              foo1 <- pure (P1foo v0 p1)

              (P1foo v1 v2) @ _3 <- pure foo1
              fooRet <- foo v2
              pure fooRet

            foo y0 =
              z0 <- pure (#undefined :: #ptr)
              x0 <- pure (#undefined :: #ptr)
              y0' <- fetch y0
              (CInt n) @ _4 <- y0'
              pure y0'
          |]
      dpe before `sameAs` after

    it "Simple" $ do
      let before = [prog|
            grinMain =
              k0 <- pure 5
              g k0

            f x y = pure x

            g z =
              k1 <- pure 0
              f k1 z
          |]

      let after = [prog|
            grinMain =
              k0 <- pure 5
              g

            f x =
              y <- pure (#undefined :: T_Int64)
              pure x

            g =
              z <- pure (#undefined :: T_Int64)
              k1 <- pure 0
              f k1
          |]
      dpe before `sameAs` after

    it "Mutually recursive" $ do
      let before = [prog|
            grinMain =
              k0 <- pure 0
              f k0 k0

            f x y =
              k1 <- pure 0
              g x k1

            g v w =
              k2 <- pure 0
              f k2 w
          |]

      let after = [prog|
            grinMain =
              k0 <- pure 0
              f

            f =
              y <- pure (#undefined :: T_Int64)
              x <- pure (#undefined :: T_Int64)
              k1 <- pure 0
              g

            g =
              w <- pure (#undefined :: T_Int64)
              v <- pure (#undefined :: T_Int64)
              k2 <- pure 0
              f
          |]
      dpe before `sameAs` after
