{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.ExtendedSyntax.BindNormalisationSpec where

import Transformations.ExtendedSyntax.BindNormalisation

import Test.Hspec

import Test.ExtendedSyntax.Old.Test
import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
      test1 =
        i1 <- pure 1
        i2 <- do
          pure 2
        pure (CPair i1 i2)

      test2 =
        i1 <- pure 1
        i2 <- do
          pure 2
        do
          pure (CPair i1 i2)

      test3 =
        do
          p <- do
            i1 <- pure 1
            do
              i2 <- do
                pure 2
              pure (CPair i1 i2)
          pure p

      test4 =
        x0 <- do
          x1 <- do
            x2 <- do
              pure 2
            pure 1
          pure 0
        pure x0

      test5 =
        a <- do
          b <- pure 1
          c <- pure 2
          pure b
        pure a
      |]
    let after = [prog|
      test1 =
        i1 <- pure 1
        i2 <- pure 2
        pure (CPair i1 i2)

      test2 =
        i1 <- pure 1
        i2 <- pure 2
        pure (CPair i1 i2)

      test3 =
        i1 <- pure 1
        i2 <- pure 2
        p <- pure (CPair i1 i2)
        pure p

      test4 =
        x2 <- pure 2
        x1 <- pure 1
        x0 <- pure 0
        pure x0

      test5 =
        b <- pure 1
        c <- pure 2
        a <- pure b
        pure a
      |]
    (bindNormalisation before) `sameAs` after

  it "case" $ do
    let before = [prog|
      test4 p =
        x <- do
          do
            case p of
              0@_1 ->
                p <- do
                  i1 <- pure 1
                  do
                    i2 <- do
                      pure 2
                    pure (CPair i1 i2)
                pure p
              #default@_2 ->
                j1 <- pure 1
                j2 <- do
                  pure 2
                pure (CPair j1 j2)
        do
          case x of
            #default@_3 ->
              do
                do
                  pure x
      |]
    let after = [prog|
      test4 p =
        x <- case p of
          0@_1 ->
            i1 <- pure 1
            i2 <- pure 2
            p <- pure (CPair i1 i2)
            pure p
          #default@_2 ->
            j1 <- pure 1
            j2 <- pure 2
            pure (CPair j1 j2)
        case x of
          #default@_3 ->
            pure x
      |]
    (bindNormalisation before) `sameAs` after
