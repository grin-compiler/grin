{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.DeadProcedureEliminationSpec where

import Transformations.Optimising.DeadProcedureElimination

import Test.Hspec
import Grin.GrinTH
import Test.Test hiding (newVar)
import Test.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
        grinMain =
          funA 1
          funB 2

        funA a = pure ()
        funB b = funC b
        funC c = pure ()

        deadFunA d = pure d
        deadFunB e = deadFunA e
      |]
    let after = [prog|
        grinMain =
          funA 1
          funB 2

        funA a = pure ()
        funB b = funC b
        funC c = pure ()
      |]
    deadProcedureElimination before `sameAs` after

  it "reference direction" $ do
    let before = [prog|
        grinMain =
          funA 1

        funA b = funB b
        funB c = pure ()

        deadFunA d = funA d
        deadFunB e = deadFunA e
      |]
    let after = [prog|
        grinMain =
          funA 1

        funA b = funB b
        funB c = pure ()
      |]
    deadProcedureElimination before `sameAs` after

  it "ignore unknown function" $ do
    let before = [prog|
        grinMain =
          funA 1
          funB 2

        deadFunA d = pure d
        deadFunB e = deadFunA e
      |]
    let after = [prog|
        grinMain =
          funA 1
          funB 2
      |]
    deadProcedureElimination before `sameAs` after

  it "dead clique" $ do
    let before = [prog|
        grinMain =
          funA 1

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
          funA 1

        funA b = funB b
        funB c = pure ()
      |]
    deadProcedureElimination before `sameAs` after
