{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.CaseCopyPropagationSpec where

import Transformations.Optimising.CaseCopyPropagation

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions
import TypeEnv
import Data.Monoid

spec :: Spec
spec = testExprContext $ \ctx -> do
  it "Example from Figure 4.26" $ do
    let teBefore = create $
          (newVar "z'" int64_t) <>
          (newVar "y'" int64_t) <>
          (newVar "x'" int64_t)
    let before = [expr|
            m0 <- store (CNone)
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CInt z')
                (CInt x') -> pure (CInt x')
            pure m0
          |]

    let teAfter = extend teBefore $
          newVar "v'" int64_t
    let after = [expr|
            m0 <- store (CNone)
            u  <- do
              v' <- do
                case v of
                  (Ffoo a)  -> y' <- foo a
                               pure y'
                  (Fbar b)  -> z' <- bar b
                               pure z'
                  (CInt x') -> pure x'
              pure (CInt v')
            pure m0
          |]
    caseCopyPropagation (ctx (teBefore, before)) `sameAs` (ctx (teAfter, after))

  it "One node has no Int tagged value" $ do
    let typeEnv = emptyTypeEnv
    let teBefore = create $
          (newVar "z'" float_t) <>
          (newVar "y'" int64_t) <>
          (newVar "x'" int64_t)
    let before = [expr|
            m0 <- store (CNone)
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CFloat z')
                (CInt x') -> pure (CInt x')
            pure m0
          |]
    let after = [expr|
            m0 <- store (CNone)
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CFloat z')
                (CInt x') -> pure (CInt x')
            pure m0
          |]
    caseCopyPropagation (ctx (teBefore, before)) `sameAs` (ctx (teBefore, after))

  it "Embedded good case" $ do
    let teBefore = create $
          (newVar "z'" int64_t) <>
          (newVar "y'" int64_t) <>
          (newVar "x'" int64_t) <>
          (newVar "z1'" int64_t) <>
          (newVar "y1'" int64_t) <>
          (newVar "x1'" int64_t)
    let before = [expr|
            m0 <- store (CNone)
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CInt z')
                (CInt x') -> u1 <- do
                               case v1 of
                                 (Ffoo a1)  -> y1' <- foo a1
                                               pure (CInt y1')
                                 (Fbar b1)  -> z1' <- bar b1
                                               pure (CInt z1')
                                 (CInt x1') -> pure (CInt x1')
                             pure (CInt x')
            pure m0
          |]
    let teAfter = extend teBefore $
          newVar "v'" int64_t <>
          newVar "v1'" int64_t
    let after = [expr|
            m0 <- store (CNone)
            u  <- do
              v' <- do
                case v of
                  (Ffoo a)  -> y' <- foo a
                               pure y'
                  (Fbar b)  -> z' <- bar b
                               pure z'
                  (CInt x') -> u1 <- do
                                 v1' <- do
                                   case v1 of
                                     (Ffoo a1)  -> y1' <- foo a1
                                                   pure y1'
                                     (Fbar b1)  -> z1' <- bar b1
                                                   pure z1'
                                     (CInt x1') -> pure x1'
                                 pure (CInt v1')
                               pure x'
              pure (CInt v')
            pure m0
          |]
    caseCopyPropagation (ctx (teBefore, before)) `sameAs` (ctx (teAfter, after))

  it "Embedded bad case" $ do
    let teBefore = create $
          newVar "z'" int64_t <>
          newVar "y'" int64_t <>
          newVar "x'" int64_t <>
          newVar "y1'" int64_t <>
          newVar "z1'" float_t <>
          newVar "x1'" int64_t
    let before = [expr|
            m0 <- store (CNone)
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CInt z')
                (CInt x') -> u1 <- do
                               case v1 of
                                 (Ffoo a1)  -> y1' <- foo a1
                                               pure (CInt y1')
                                 (Fbar b1)  -> z1' <- bar b1
                                               pure (CFloat z1')
                                 (CInt x1') -> pure (CInt x1')
                             pure (CInt x')
            pure m0
          |]
    let teAfter = extend teBefore $
          newVar "v'" int64_t
    let after = [expr|
            m0 <- store (CNone)
            u  <- do
              v' <- do
                case v of
                  (Ffoo a)  -> y' <- foo a
                               pure y'
                  (Fbar b)  -> z' <- bar b
                               pure z'
                  (CInt x') -> u1 <- do
                                 case v1 of
                                   (Ffoo a1)  -> y1' <- foo a1
                                                 pure (CInt y1')
                                   (Fbar b1)  -> z1' <- bar b1
                                                 pure (CFloat z1')
                                   (CInt x1') -> pure (CInt x1')
                               pure x'
              pure (CInt v')
            pure m0
          |]
    caseCopyPropagation (ctx (teBefore, before)) `sameAs` (ctx (teAfter, after))

  it "Leave the outher, transform the inner" $ do
    let teBefore = create $
          newVar "z'" float_t <>
          newVar "y'" int64_t <>
          newVar "x'" int64_t <>
          newVar "y1'" int64_t <>
          newVar "z1'" int64_t <>
          newVar "x1'" int64_t
    let before = [expr|
            m0 <- store (CNone)
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CFloat z')
                (CInt x') -> u1 <- do
                               case v1 of
                                 (Ffoo a1)  -> y1' <- foo a1
                                               pure (CInt y1')
                                 (Fbar b1)  -> z1' <- bar b1
                                               pure (CInt z1')
                                 (CInt x1') -> pure (CInt x1')
                             pure (CInt x')
            pure m0
          |]
    let teAfter = extend teBefore $
          newVar "v1'" int64_t
    let after = [expr|
            m0 <- store (CNone)
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CFloat z')
                (CInt x') -> u1 <- do
                               v1' <- do
                                 case v1 of
                                   (Ffoo a1)  -> y1' <- foo a1
                                                 pure y1'
                                   (Fbar b1)  -> z1' <- bar b1
                                                 pure z1'
                                   (CInt x1') -> pure x1'
                               pure (CInt v1')
                             pure (CInt x')
            pure m0
          |]
    caseCopyPropagation (ctx (teBefore, before)) `sameAs` (ctx (teAfter, after))

  it "last expression is a case" $ do
    let teBefore = create $
          newVar "ax'" int64_t
    let before =
          [expr|
              l2 <- eval l
              case l2 of
                (CNil) -> pure (CInt 0)
                (CCons x xs) -> (CInt x') <- eval x
                                (CInt s') <- sum xs
                                ax' <- _prim_int_add x' s'
                                pure (CInt ax')
          |]
    let teAfter = extend teBefore $
          newVar "l2'" int64_t
    let after =
          [expr|
              l2 <- eval l
              l2' <- do
                case l2 of
                  (CNil) -> pure 0
                  (CCons x xs) -> (CInt x') <- eval x
                                  (CInt s') <- sum xs
                                  ax' <- _prim_int_add x' s'
                                  pure ax'
              pure (CInt l2')
          |]
    caseCopyPropagation (ctx (teBefore, before)) `sameAs` (ctx (teAfter, after))

runTests :: IO ()
runTests = hspec spec
