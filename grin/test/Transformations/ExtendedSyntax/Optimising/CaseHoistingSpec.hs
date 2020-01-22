{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.CaseHoistingSpec where

import Transformations.ExtendedSyntax.Optimising.CaseHoisting

import Test.Hspec

import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.TypeCheck
import Test.ExtendedSyntax.Assertions
import Transformations.ExtendedSyntax.Names (ExpChanges(..))


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "last case" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              (CNil)        @ alt1 -> pure (CNil)
              (CCons a1 b1) @ alt2 -> pure (CCons a1 b1)
            case u of
              (CNil)        @ alt3 -> pure alt3
              (CCons a2 b2) @ alt4 -> pure (CNil)
      |]
    let after = [prog|
          grinMain =
            v <- pure (CNil)
            case v of
              (CNil) @ alt1 ->
                u.0 <- do
                  pure (CNil)
                alt3.0 <- pure u.0
                pure alt3.0
              (CCons a1 b1) @ alt2 ->
                u.1 <- do
                  pure (CCons a1 b1)
                alt4.0 <- pure u.1
                pure (CNil)
      |]
    caseHoisting (inferTypeEnv before) before `sameAs` (after, NewNames)

  it "middle case" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              (CNil)        @ alt1 -> pure (CNil)
              (CCons a1 b1) @ alt2 -> pure (CCons a1 b1)
            r <- case u of
              (CNil)        @ alt3 -> pure 1
              (CCons a2 b2) @ alt4 -> pure 2
            pure r
      |]
    let after = [prog|
          grinMain =
            v <- pure (CNil)
            r <- case v of
              (CNil) @ alt1 ->
                u.0 <- do
                  pure (CNil)
                alt3.0 <- pure u.0
                pure 1
              (CCons a1 b1) @ alt2 ->
                u.1 <- do
                  pure (CCons a1 b1)
                alt4.0 <- pure u.1
                pure 2
            pure r
      |]
    caseHoisting (inferTypeEnv before) before `sameAs` (after, NewNames)

  it "default pattern" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              (CNil)        @ alt1 -> pure (CNil)
              (CCons a1 b1) @ alt2 -> pure (CCons a1 b1)
            r <- case u of
              (CNil)        @ alt3 -> pure (CNil)
              #default      @ alt4 -> pure alt4
            pure r
      |]
    let after = [prog|
          grinMain =
            v <- pure (CNil)
            r <- case v of
              (CNil) @ alt1 ->
                u.0 <- do
                  pure (CNil)
                alt3.0 <- pure u.0
                pure (CNil)
              (CCons a1 b1) @ alt2 ->
                u.1 <- do
                  pure (CCons a1 b1)
                alt4.0 <- pure u.1
                pure alt4.0
            pure r
      |]
    caseHoisting (inferTypeEnv before) before `sameAs` (after, NewNames)

  it "case chain + no code duplication" $ do
    let before = [prog|
          grinMain =
            v <- pure 1
            u <- case v of
              0 @ alt1 -> pure (CNil)
              1 @ alt2 -> pure (CCons v v)
            r <- case u of
              (CNil)   @ alt3 -> pure (CEmpty)
              #default @ alt4 -> pure u
            q <- case r of
              (CVoid) @ alt5 ->
                pure (CEmpty)
              #default @ alt6 ->
                k0 <- pure 777
                _1 <- _prim_int_print k0
                pure r
            pure q
      |]
    let after = [prog|
          grinMain =
            v <- pure 1
            r <- case v of
              0 @ alt1 ->
                u.0 <- do
                  pure (CNil)
                alt3.0 <- pure u.0
                pure (CEmpty)
              1 @ alt2 ->
                u.1 <- do
                  pure (CCons v v)
                alt4.0 <- pure u.1
                pure u.1
            q <- case r of
              (CVoid) @ alt5 ->
                pure (CEmpty)
              #default @ alt6 ->
                k0 <- pure 777
                _1 <- _prim_int_print k0
                pure r
            pure q
      |]
    caseHoisting (inferTypeEnv before) before `sameAs` (after, NewNames)

  it "default chain" $ do
    let before = [prog|
          grinMain =
            v <- pure 1
            u <- case v of
              0 @ alt1 -> pure (CNil)
              1 @ alt2 -> pure (CCons v v)
            r <- case u of
              #default @ alt3 -> pure u
            q <- case r of
              #default @ alt4 -> pure r
            pure q
      |]
    let after = [prog|
          grinMain =
            v <- pure 1
            u <- case v of
              0 @ alt1 ->
                pure (CNil)
              1 @ alt2 ->
                pure (CCons v v)
            q <- case u of
              #default @ alt3 ->
                r.0 <- do
                  pure u
                alt4.0 <- pure r.0
                pure r.0
            pure q
      |]
    caseHoisting (inferTypeEnv before) before `sameAs` (after, NewNames)

  it "ignore non linear variable" $ do
    let before = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              #default @ alt1 -> pure v
            r <- case u of
              #default @ alt2 -> pure u
            x <- pure u
            pure r
      |]
    let after = [prog|
          grinMain =
            v <- pure (CNil)
            u <- case v of
              #default @ alt1 -> pure v
            r <- case u of
              #default @ alt2 -> pure u
            x <- pure u
            pure r
      |]
    caseHoisting (inferTypeEnv before) before `sameAs` (after, NoChange)
