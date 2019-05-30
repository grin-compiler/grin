{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.Simplifying.ProducerNameIntroductionSpec where

import Transformations.BindNormalisation
import Transformations.Simplifying.ProducerNameIntroduction
import Transformations.Names (ExpChanges(..))

import Test.Test
import Grin.TH
import Grin.Grin
import Test.Hspec
import Test.Assertions
import Data.Bifunctor ( first )


runTests :: IO ()
runTests = hspec spec

pni :: Exp -> (Exp, ExpChanges)
pni = first bindNormalisation . producerNameIntroduction

spec :: Spec
spec = do
  it "store" $ do
    let before = [prog|
      grinMain =
        p <- store (CInt 5)
        pure p
      |]
    let after = [prog|
      grinMain =
        v.0 <- pure (CInt 5)
        p <- store v.0
        pure p
      |]
    (pni before) `sameAs` (after, NewNames)

  it "update" $ do
    let before = [prog|
      grinMain =
        p <- store (CInt 5)
        update p (CInt 6)
        pure p
      |]
    let after = [prog|
      grinMain =
        v.0 <- pure (CInt 5)
        p <- store v.0
        v.1 <- pure (CInt 6)
        update p v.1
        pure p
      |]
    (pni before) `sameAs` (after, NewNames)

  it "pure" $ do
    let before = [prog|
      grinMain =
        n <- pure (CInt 5)
        pure n
      |]
    let after = [prog|
      grinMain =
        v.0 <- pure (CInt 5)
        n <- pure v.0
        pure n
      |]
    (pni before) `sameAs` (after, NewNames)

  it "store_vartag" $ do
    let before = [prog|
      grinMain =
        t <- pure CInt
        p <- store (t 5)
        pure p
      |]
    let after = [prog|
      grinMain =
        t <- pure CInt
        v.0 <- pure (t 5)
        p <- store v.0
        pure p
      |]
    (pni before) `sameAs` (after, NewNames)

  it "update_vartag" $ do
    let before = [prog|
      grinMain =
        t <- pure CInt
        p <- store (t 5)
        update p (t 6)
        pure p
      |]
    let after = [prog|
      grinMain =
        t <- pure CInt
        v.0 <- pure (t 5)
        p <- store v.0
        v.1 <- pure (t 6)
        update p v.1
        pure p
      |]
    (pni before) `sameAs` (after, NewNames)

  it "pure_vartag" $ do
    let before = [prog|
      grinMain =
        t <- pure CInt
        n <- pure (t 5)
        pure n
      |]
    let after = [prog|
      grinMain =
        t <- pure CInt
        v.0 <- pure (t 5)
        n <- pure v.0
        pure n
      |]
    (pni before) `sameAs` (after, NewNames)

  it "pure_undefined" $ do
    let before = [prog|
      f =
        pure (#undefined :: T_Bool)
      |]
    let after = [prog|
      f =
        v.0 <- pure (#undefined :: T_Bool)
        pure v.0
      |]
    (pni before) `sameAs` (after, NewNames)

  it "store_undefined" $ do
    let before = [prog|
      f =
        store (#undefined :: {CNil[]})
      |]
    let after = [prog|
      f =
        v.0 <- pure (#undefined :: {CNil[]})
        store v.0
      |]
    (pni before) `sameAs` (after, NewNames)

  it "update_undefined" $ do
    let before = [prog|
      f p =
        update p (#undefined :: {CNil[]})
      |]
    let after = [prog|
      f p =
        v.0 <- pure (#undefined :: {CNil[]})
        update p v.0
      |]
    (pni before) `sameAs` (after, NewNames)
