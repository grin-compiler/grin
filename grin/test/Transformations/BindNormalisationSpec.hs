{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.BindNormalisationSpec where

import Transformations.BindNormalisation

import Test
import GrinTH
import Test.Hspec
import Assertions


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
      |]
    (bindNormalisation before) `sameAs` after

  it "case" $ do
    let before = [prog|
      test4 p =
        x <- do
          do
            case p of
              0 ->
                p <- do
                  i1 <- pure 1
                  do
                    i2 <- do
                      pure 2
                    pure (CPair i1 i2)
                pure p
              #default ->
                j1 <- pure 1
                j2 <- do
                  pure 2
                pure (CPair j1 j2)
        do
          case x of
            #default ->
              do
                do
                  pure x
      |]
    let after = [prog|
      test4 p =
        x <- case p of
          0 ->
            i1 <- pure 1
            i2 <- pure 2
            p <- pure (CPair i1 i2)
            pure p
          #default ->
            j1 <- pure 1
            j2 <- pure 2
            pure (CPair j1 j2)
        case x of
          #default ->
            pure x
      |]
    (bindNormalisation before) `sameAs` after
