{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.CaseHoistingSpec where

import Transformations.Optimising.CaseHoisting

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions
import TypeCheck


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "last case" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              (CNil)        -> pure (CNil)
              (CCons a1 b1) -> pure (CCons a1 b1)
            case u of
              (CNil)        -> pure 1
              (CCons a2 b2) -> pure 2
      |]
    let after = [prog|
          grinMain =
            v <- pure (CNil)
            case v of
              (CNil) ->
                u.0 <- do
                  pure (CNil)
                (CNil) <- pure u.0
                pure 1
              (CCons a1 b1) ->
                u.1 <- do
                  pure (CCons a1 b1)
                (CCons a2.0 b2.0) <- pure u.1
                pure 2
      |]
    snd (caseHoisting (inferTypeEnv before, before)) `sameAs` after

  it "middle case" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              (CNil)        -> pure (CNil)
              (CCons a1 b1) -> pure (CCons a1 b1)
            r <- case u of
              (CNil)        -> pure 1
              (CCons a2 b2) -> pure 2
            pure r
      |]
    let after = [prog|
          grinMain =
            v <- pure (CNil)
            r <- case v of
              (CNil) ->
                u.0 <- do
                  pure (CNil)
                (CNil) <- pure u.0
                pure 1
              (CCons a1 b1) ->
                u.1 <- do
                  pure (CCons a1 b1)
                (CCons a2.0 b2.0) <- pure u.1
                pure 2
            pure r
      |]
    snd (caseHoisting (inferTypeEnv before, before)) `sameAs` after
