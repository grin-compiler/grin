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

  it "default pattern" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              (CNil)        -> pure (CNil)
              (CCons a1 b1) -> pure (CCons a1 b1)
            r <- case u of
              (CNil)        -> pure 1
              #default      -> pure 2
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
                pure 2
            pure r
      |]
    snd (caseHoisting (inferTypeEnv before, before)) `sameAs` after

  it "case chain + no code duplication" $ do
    let before = [prog|
          grinMain =
            v <- pure 1
            u <- case v of
              0 -> pure (CNil)
              1 -> pure (CCons 3 4)
            r <- case u of
              (CNil)        -> pure (CEmpty)
              #default      -> pure u
            q <- case r of
              (CVoid)       -> pure (CEmpty)
              #default      ->  _prim_int_print 777
                                pure r
            pure q
      |]
    let after = [prog|
          grinMain =
            v <- pure 1
            r <- case v of
              0 ->
                u.0 <- do
                  pure (CNil)
                (CNil) <- pure u.0
                pure (CEmpty)
              1 ->
                u.1 <- do
                  pure (CCons 3 4)
                pure u.1
            q <- case r of
              (CVoid)       -> pure (CEmpty)
              #default      ->  _prim_int_print 777
                                pure r
            pure q
      |]
    snd (caseHoisting (inferTypeEnv before, before)) `sameAs` after

  it "default chain" $ do
    let before = [prog|
          grinMain =
            v <- pure 1
            u <- case v of
              0 -> pure (CNil)
              1 -> pure (CCons 3 4)
            r <- case u of
              #default  -> pure u
            q <- case r of
              #default  -> pure r
            pure q
      |]
    let after = [prog|
          grinMain =
            v <- pure 1
            u <- case v of
              0 ->
                pure (CNil)
              1 ->
                pure (CCons 3 4)
            q <- case u of
              #default ->
                r.0 <- do
                  pure u
                pure r.0
            pure q
      |]
    snd (caseHoisting (inferTypeEnv before, before)) `sameAs` after

  it "ignore non linear variable" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              #default  -> pure v
            r <- case u of
              #default  -> pure u
            x <- pure u
            pure r
      |]
    let after = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              #default  -> pure v
            r <- case u of
              #default  -> pure u
            x <- pure u
            pure r
      |]
    snd (caseHoisting (inferTypeEnv before, before)) `sameAs` after
