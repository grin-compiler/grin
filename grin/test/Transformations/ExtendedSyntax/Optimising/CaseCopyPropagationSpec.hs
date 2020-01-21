{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.ExtendedSyntax.Optimising.CaseCopyPropagationSpec where

import Transformations.ExtendedSyntax.Optimising.CaseCopyPropagation

import Data.Monoid

import Test.Hspec

import Test.ExtendedSyntax.New.Test hiding (newVar)
import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.TypeEnv
import Grin.ExtendedSyntax.Pretty
import Transformations.ExtendedSyntax.Names (ExpChanges(..))


ctxs :: [TestExpContext]
ctxs =
  [ emptyCtx
  , lastBindR
  , firstAlt
  , middleAlt
  , lastAlt
  ]


spec :: Spec
spec = testExprContextIn ctxs $ \ctx -> do

  it "Example from Figure 4.26" $ do
    let teBefore = create $
          (newVar "z'" int64_t) <>
          (newVar "y'" int64_t) <>
          (newVar "x'" int64_t)
    let before = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- case v of
              (Ffoo a) @ alt0 ->
                y' <- foo a
                pure (CInt y')
              (Fbar b) @ alt1 ->
                z' <- bar b
                pure (CInt z')
              (CInt x') @ alt2 ->
                pure (CInt x')
            pure m0
          |]

    let teAfter = extend teBefore $
          newVar "v'" int64_t
    let after = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- do
              ccp.0 <- case v of
                (Ffoo a) @ alt0 ->
                  y' <- foo a
                  pure y'
                (Fbar b) @ alt1 ->
                  z' <- bar b
                  pure z'
                (CInt x') @ alt2 ->
                  pure x'
              pure (CInt ccp.0)
            pure m0
          |]
    -- TODO: Inspect type env
    (caseCopyPropagation (snd (ctx (teBefore, before)))) `sameAs` ((snd (ctx (teAfter, after))), NewNames)
    --(snd (ctx (teBefore, before))) `sameAs` (snd (ctx (teAfter, after)))

  it "One node has no Int tagged value" $ do
    let typeEnv = emptyTypeEnv
    let teBefore = create $
          (newVar "z'" float_t) <>
          (newVar "y'" int64_t) <>
          (newVar "x'" int64_t)
    let before = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- do
              case v of
                (Ffoo a) @ alt0 ->
                  y' <- foo a
                  pure (CInt y')
                (Fbar b) @ alt1 ->
                  z' <- bar b
                  pure (CFloat z')
                (CInt x') @ alt2 ->
                  pure (CInt x')
            pure m0
          |]
    let after = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- do
              case v of
                (Ffoo a) @ alt0 ->
                  y' <- foo a
                  pure (CInt y')
                (Fbar b) @ alt1 ->
                  z' <- bar b
                  pure (CFloat z')
                (CInt x') @ alt2 ->
                  pure (CInt x')
            pure m0
          |]
    (caseCopyPropagation (snd (ctx (teBefore, before)))) `sameAs` ((snd (ctx (teBefore, after))), NoChange)

  it "Embedded good case" $ do
    -- pendingWith "doesn't unbox outer case"
    let teBefore = create $
          (newVar "z'" int64_t) <>
          (newVar "y'" int64_t) <>
          (newVar "x'" int64_t) <>
          (newVar "z1'" int64_t) <>
          (newVar "y1'" int64_t) <>
          (newVar "x1'" int64_t)
    let before = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- case v of
              (Ffoo a) @ alt0 ->
                y' <- foo a
                pure (CInt y')
              (Fbar b) @ alt1 ->
                z' <- bar b
                pure (CInt z')
              (CInt x') @ alt2 ->
                u1 <- case v1 of
                  (Ffoo a1) @ alt20 ->
                    y1' <- foo a1
                    pure (CInt y1')
                  (Fbar b1) @ alt21 ->
                    z1' <- bar b1
                    pure (CInt z1')
                  (CInt x1') @ alt22 ->
                    pure (CInt x1')
                pure (CInt x')
            pure m0
          |]
    let teAfter = extend teBefore $
          newVar "v'" int64_t <>
          newVar "v1'" int64_t
    let after = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- do
              ccp.1 <- case v of
                (Ffoo a) @ alt0 ->
                  y' <- foo a
                  pure y'
                (Fbar b) @ alt1 ->
                  z' <- bar b
                  pure z'
                (CInt x') @ alt2 ->
                  u1 <- do
                    ccp.0 <- case v1 of
                      (Ffoo a1) @ alt20 ->
                        y1' <- foo a1
                        pure y1'
                      (Fbar b1) @ alt21 ->
                        z1' <- bar b1
                        pure z1'
                      (CInt x1') @ alt22 ->
                        pure x1'
                    pure (CInt ccp.0)
                  pure x'
              pure (CInt ccp.1)
            pure m0
          |]
    (caseCopyPropagation (snd (ctx (teBefore, before)))) `sameAs` ((snd (ctx (teAfter, after))), NewNames)

  it "Embedded bad case" $ do
    let teBefore = create $
          newVar "z'" int64_t <>
          newVar "y'" int64_t <>
          newVar "x'" int64_t <>
          newVar "y1'" int64_t <>
          newVar "z1'" float_t <>
          newVar "x1'" int64_t
    let before = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- case v of
              (Ffoo a) @ alt0 ->
                y' <- foo a
                pure (CInt y')
              (Fbar b) @ alt1 ->
                z' <- bar b
                pure (CInt z')
              (CInt x') @ alt2 ->
                u1 <- do
                  case v1 of
                    (Ffoo a1) @ alt20 ->
                      y1' <- foo a1
                      pure (CInt y1')
                    (Fbar b1) @ alt21 ->
                      z1' <- bar b1
                      pure (CFloat z1')
                    (CInt x1') @ alt22 ->
                      pure (CInt x1')
                pure (CInt x')
            pure m0
          |]
    let teAfter = extend teBefore $
          newVar "v'" int64_t
    let after = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- do
              ccp.0 <- case v of
                (Ffoo a) @ alt0 ->
                  y' <- foo a
                  pure y'
                (Fbar b) @ alt1 ->
                  z' <- bar b
                  pure z'
                (CInt x') @ alt2 ->
                  u1 <- do
                    case v1 of
                      (Ffoo a1) @ alt20 ->
                        y1' <- foo a1
                        pure (CInt y1')
                      (Fbar b1) @ alt21 ->
                        z1' <- bar b1
                        pure (CFloat z1')
                      (CInt x1') @ alt22 ->
                        pure (CInt x1')
                  pure x'
              pure (CInt ccp.0)
            pure m0
          |]
    (caseCopyPropagation (snd (ctx (teBefore, before)))) `sameAs` ((snd (ctx (teAfter, after))), NewNames)

  it "Leave the outer, transform the inner" $ do
    let teBefore = create $
          newVar "z'" float_t <>
          newVar "y'" int64_t <>
          newVar "x'" int64_t <>
          newVar "y1'" int64_t <>
          newVar "z1'" int64_t <>
          newVar "x1'" int64_t
    let before = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- do
              case v of
                (Ffoo a) @ alt0 ->
                  y' <- foo a
                  pure (CInt y')
                (Fbar b) @ alt1 ->
                  z' <- bar b
                  pure (CFloat z')
                (CInt x') @ alt2 ->
                  u1 <- case v1 of
                    (Ffoo a1) @ alt20 ->
                      y1' <- foo a1
                      pure (CInt y1')
                    (Fbar b1) @ alt21 ->
                      z1' <- bar b1
                      pure (CInt z1')
                    (CInt x1') @ alt22 ->
                      pure (CInt x1')
                  pure (CInt x')
            pure m0
          |]
    let teAfter = extend teBefore $
          newVar "v1'" int64_t
    let after = [expr|
            n0 <- pure (CNone)
            m0 <- store n0
            u  <- do
              case v of
                (Ffoo a) @ alt0 ->
                  y' <- foo a
                  pure (CInt y')
                (Fbar b) @ alt1 ->
                  z' <- bar b
                  pure (CFloat z')
                (CInt x') @ alt2 ->
                  u1 <- do
                    ccp.0 <- case v1 of
                      (Ffoo a1) @ alt20 ->
                        y1' <- foo a1
                        pure y1'
                      (Fbar b1) @ alt21 ->
                        z1' <- bar b1
                        pure z1'
                      (CInt x1') @ alt22 ->
                        pure x1'
                    pure (CInt ccp.0)
                  pure (CInt x')
            pure m0
          |]

    (caseCopyPropagation (snd (ctx (teBefore, before)))) `sameAs` ((snd (ctx (teAfter, after))), NewNames)

  it "last expression is a case" $ do
    let teBefore = create $
          newVar "ax'" int64_t
    let before =
          [expr|
              l2 <- eval l
              case l2 of
                (CNil) @ alt0 ->
                  k0 <- pure 0
                  pure (CInt k0)
                (CCons x xs) @ alt1 ->
                  (CInt x') @ v0 <- eval x
                  (CInt s') @ v1 <- sum xs
                  ax' <- _prim_int_add x' s'
                  pure (CInt ax')
          |]
    let teAfter = extend teBefore $
          newVar "l2'" int64_t
    let after =
          [expr|
              l2 <- eval l
              do ccp.0 <- case l2 of
                   (CNil) @ alt0 ->
                     k0 <- pure 0
                     pure k0
                   (CCons x xs) @ alt1 ->
                     (CInt x') @ v0 <- eval x
                     (CInt s') @ v1 <- sum xs
                     ax' <- _prim_int_add x' s'
                     pure ax'
                 pure (CInt ccp.0)
          |]
    (caseCopyPropagation (snd (ctx (teBefore, before)))) `sameAs` ((snd (ctx (teAfter, after))), NewNames)

runTests :: IO ()
runTests = hspec spec
