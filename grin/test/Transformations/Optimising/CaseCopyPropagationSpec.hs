{-# LANGUAGE TypeApplications, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Transformations.Optimising.CaseCopyPropagationSpec where

import Transformations.Optimising.CaseCopyPropagation

import Test.Hspec
import Grin
import GrinTH
import Test
import Assertions
import ParseGrin


spec :: Spec
spec = do

  it "Example from Figure 4.26" $ do
    let before = [expr|
            m0 <- store 3
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CInt z')
                (CInt x') -> pure (CInt x')
            pure m0
          |]
    let after = [expr|
            m0 <- store 3
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
    caseCopyPropagation before `sameAs` after

  it "One node has no Int tagged value" $ do
    let before = [expr|
            m0 <- store 3
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
            m0 <- store 3
            u  <- do
              case v of
                (Ffoo a)  -> y' <- foo a
                             pure (CInt y')
                (Fbar b)  -> z' <- bar b
                             pure (CFloat z')
                (CInt x') -> pure (CInt x')
            pure m0
          |]
    caseCopyPropagation before `sameAs` after

  it "Embedded good case" $ do
    let before = [expr|
            m0 <- store 3
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
    let after = [expr|
            m0 <- store 3
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
    caseCopyPropagation before `sameAs` after

  it "Embedded bad case" $ do
    let before = [expr|
            m0 <- store 3
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

    let after = [expr|
            m0 <- store 3
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
    caseCopyPropagation before `sameAs` after

  it "Leave the outher, transform the inner" $ do
    let before = [expr|
            m0 <- store 3
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
    let after = [expr|
            m0 <- store 3
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
    caseCopyPropagation before `sameAs` after

  it "last expression is a case" $ do
    let before =
          [def|
            sum l =
              l2 <- eval l
              case l2 of
                (CNil) -> pure (CInt 0)
                (CCons x xs) -> (CInt x') <- eval x
                                (CInt s') <- sum xs
                                ax' <- _prim_int_add x' s'
                                pure (CInt ax')
          |]
    let after =
          [def|
            sum l =
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
    caseCopyPropagation before `sameAs` after

runTests :: IO ()
runTests = hspec spec
