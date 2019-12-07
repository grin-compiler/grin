{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.Simplifying.BindingPatternSimplificationSpec where

import Transformations.Simplifying.BindingPatternSimplification
import Transformations.BindNormalisation
import Transformations.Names (ExpChanges(..))

import Test.Test
import Grin.TH
import Grin.Grin
import Test.Hspec
import Test.Assertions
import Data.Bifunctor ( first )


runTests :: IO ()
runTests = hspec spec

bps :: Exp -> (Exp, ExpChanges)
bps = first bindNormalisation . bindingPatternSimplification

spec :: Spec
spec = do

  it "pure_to_lit" $ do

    let before = [prog|
      grinMain p =
        5 <- pure 5
        pure 0
      |]

    let after = [prog|
      grinMain p =
        p.0 <- pure 5
        5 <- pure p.0
        pure 0
      |]
    (bps $ before) `sameAs` (after, NewNames)

  it "pure_to_node" $ do

    let before = [prog|
      grinMain p =
        (CInt n) <- pure (CInt 5)
        pure n
      |]

    let after = [prog|
      grinMain p =
        p.0 <- pure (CInt 5)
        (CInt n) <- pure p.0
        pure n
      |]
    (bps $ before) `sameAs` (after, NewNames)

  it "fun_to_lit" $ do

    let before = [prog|
      f x = pure x

      grinMain p =
        5 <- f 5
        pure 0
      |]

    let after = [prog|
      f x = pure x

      grinMain p =
        p.0 <- f 5
        5 <- pure p.0
        pure 0
      |]
    (bps $ before) `sameAs` (after, NewNames)

  it "fun_to_node" $ do

    let before = [prog|
      f x = pure x

      grinMain p =
        y <- pure (CInt 5)
        (CInt n) <- f y
        pure n
      |]

    let after = [prog|
      f x = pure x

      grinMain p =
        y <- pure (CInt 5)
        p.0 <- f y
        (CInt n) <- pure p.0
        pure n
      |]
    (bps $ before) `sameAs` (after, NewNames)

  it "fetch_to_node" $ do

    let before = [prog|
      grinMain p =
        (CInt n) <- fetch x
        pure n
      |]

    let after = [prog|
      grinMain p =
        p.0 <- fetch x
        (CInt n) <- pure p.0
        pure n
      |]
    (bps $ before) `sameAs` (after, NewNames)

  it "case_to_lit" $ do

    let before = [prog|
      grinMain p =
        0 <- case 0 of
          0 -> pure 0
        pure 0
      |]

    let after = [prog|
      grinMain p =
        p.0 <- pure 0
        p.1 <- case p.0 of
          0 -> pure 0
        0 <- pure p.1
        pure 0
      |]
    (bps $ before) `sameAs` (after, NewNames)

  it "case_to_node" $ do

    let before = [prog|
      grinMain p =
        (CInt 0) <- case (CUnit) of
          (CUnit) -> pure (CInt 0)
        pure 0
      |]

    let after = [prog|
      grinMain p =
        p.0 <- pure (CUnit)
        p.1 <- case p.0 of
          (CUnit) -> pure (CInt 0)
        (CInt 0) <- pure p.1
        pure 0
      |]
    (bps $ before) `sameAs` (after, NewNames)

  it "case_to_node_complex" $ do

    let before = [prog|
      grinMain p =
        (CInt2 0) <- case (CBool #True) of
          (CBool b) ->
            (CInt n) <- pure (CInt 0)
            pure (CInt2 n)
        pure 0
      |]

    let after = [prog|
      grinMain p =
        p.1 <- pure (CBool #True)
        p.2 <- case p.1 of
          (CBool b) ->
            p.0 <- pure (CInt 0)
            (CInt n) <- pure p.0
            pure (CInt2 n)
        (CInt2 0) <- pure p.2
        pure 0
      |]
    (bps $ before) `sameAs` (after, NewNames)


