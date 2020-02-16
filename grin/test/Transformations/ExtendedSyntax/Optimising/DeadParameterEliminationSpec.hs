{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.DeadParameterEliminationSpec where

import Transformations.ExtendedSyntax.Optimising.DeadParameterElimination

import Test.Hspec

import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
          funA a b = pure b
          funB c =
            k0 <- pure 1
            funA c k0
      |]
    let after = [prog|
          funA b = pure b
          funB c =
            k0 <- pure 1
            funA k0
      |]
    deadParameterElimination before `sameAs` after

  it "Pnode + Fnode ; val - lpat - cpat" $ do
    let before = [prog|
          funA a b = pure b
          funB c =
            k0 <- pure 1
            funA c k0

          eval p =
            v <- fetch p
            case v of
              (FfunB c1) @ alt1 -> funB c1
              (FfunA a1 b1) @ alt2 ->
                (FfunA a2 b2) @ _1 <- pure (FfunA a1 b1)
                funA a2 b2
              (P2funA) @ alt3 ->
                (P2funA) @ _2 <- pure (P2funA)
                pure (P2funA)
              (P1funA a3) @ alt4 ->
                (P1funA a4) @ _3 <- pure (P1funA a3)
                pure (P1funA a4)
              (P0funA a5 b5) @ alt5 ->
                (P0funA a6 b6) @ _4 <- pure (P0funA a5 b5)
                pure (P0funA a6 b6)
      |]
    let after = [prog|
          funA b = pure b
          funB c =
            k0 <- pure 1
            funA k0

          eval p =
            v <- fetch p
            case v of
              (FfunB c1) @ alt1 -> funB c1
              (FfunA b1) @ alt2 ->
                (FfunA b2) @ _1 <- pure (FfunA b1)
                funA b2
              (P2funA) @ alt3 ->
                (P2funA) @ _2 <- pure (P2funA)
                pure (P2funA)
              (P1funA) @ alt4 ->
                (P1funA) @ _3 <- pure (P1funA)
                pure (P1funA)
              (P0funA b5) @ alt5 ->
                (P0funA b6) @ _4 <- pure (P0funA b5)
                pure (P0funA b6)
      |]
    deadParameterElimination before `sameAs` after
