{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.DeadParameterEliminationSpec where

import Transformations.Optimising.DeadParameterElimination

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
          funA a b = pure b
          funB c = funA c 1
      |]
    let after = [prog|
          funA b = pure b
          funB c = funA 1
      |]
    deadParameterElimination before `sameAs` after

  it "Pnode + Fnode ; val - lpat - cpat" $ do
    let before = [prog|
          funA a b = pure b
          funB c = funA c 1

          eval p =
            v <- fetch p
            case v of
              (FfunB c1) -> funB c1
              (FfunA a1 b1) ->
                (FfunA a2 b2) <- pure (FfunA a1 b1)
                funA a2 b2
              (P2funA) ->
                (P2funA) <- pure (P2funA)
                pure (P2funA)
              (P1funA a3) ->
                (P1funA a4) <- pure (P1funA a3)
                pure (P1funA a4)
              (P0funA a5 b5) ->
                (P0funA a6 b6) <- pure (P0funA a5 b5)
                pure (P0funA a6 b6)
      |]
    let after = [prog|
          funA b = pure b
          funB c = funA 1

          eval p =
            v <- fetch p
            case v of
              (FfunB c1) -> funB c1
              (FfunA b1) ->
                (FfunA b2) <- pure (FfunA b1)
                funA b2
              (P2funA) ->
                (P2funA) <- pure (P2funA)
                pure (P2funA)
              (P1funA) ->
                (P1funA) <- pure (P1funA)
                pure (P1funA)
              (P0funA b5) ->
                (P0funA b6) <- pure (P0funA b5)
                pure (P0funA b6)
      |]
    deadParameterElimination before `sameAs` after
