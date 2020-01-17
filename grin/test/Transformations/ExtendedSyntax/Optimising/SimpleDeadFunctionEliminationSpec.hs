{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.SimpleDeadFunctionEliminationSpec where

import Transformations.Optimising.SimpleDeadFunctionElimination

import Test.Hspec
import Grin.TH
import Test.Test hiding (newVar)
import Test.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
        grinMain =
          x <- pure 1
          funA x
          funB x

        funA a = pure ()
        funB b = funC b
        funC c = pure ()

        deadFunA d = pure d
        deadFunB e = deadFunA e
      |]
    let after = [prog|
        grinMain =
          x <- pure 1
          funA x
          funB x

        funA a = pure ()
        funB b = funC b
        funC c = pure ()
      |]
    simpleDeadFunctionElimination before `sameAs` after

  it "reference direction" $ do
    let before = [prog|
        grinMain =
          x <- pure 1
          funA x

        funA b = funB b
        funB c = pure ()

        deadFunA d = funA d
        deadFunB e = deadFunA e
      |]
    let after = [prog|
        grinMain =
          x <- pure 1
          funA x

        funA b = funB b
        funB c = pure ()
      |]
    simpleDeadFunctionElimination before `sameAs` after

  it "ignore unknown function" $ do
    let before = [prog|
        grinMain =
          x <- pure 1
          funA x
          funB x

        deadFunA d = pure d
        deadFunB e = deadFunA e
      |]
    let after = [prog|
        grinMain =
          x <- pure 1
          funA x
          funB x
      |]
    simpleDeadFunctionElimination before `sameAs` after

  it "dead clique" $ do
    let before = [prog|
        grinMain =
          x <- pure 1
          funA x

        funA b = funB b
        funB c = pure ()

        deadFunA d =
          v1 <- funA d
          deadFunB d

        deadFunB e =
          v2 <- funA d
          deadFunA e
      |]
    let after = [prog|
        grinMain =
          x <- pure 1
          funA x

        funA b = funB b
        funB c = pure ()
      |]
    simpleDeadFunctionElimination before `sameAs` after
