{-# LANGUAGE TypeApplications, OverloadedStrings, LambdaCase #-}

module Transformations.Simplifying.SplitFetchSpec where

import Control.Monad
import Test.Hspec
import Test hiding (asVal)
import Test.QuickCheck.Property

import Free
import Grin
import Test
import Assertions
import Transformations.Simplifying.SplitFetch


spec :: Spec
spec = do
  it "Example from Figure 4.13" $ do
    before <- buildExpM $
      "l1" <=: store @Int 3                                       $
      (VarTagNode "t" ["a1", "a2", "a3"]) <=: fetch "p" Nothing   $
      "l2" <=: store @Int 4                                       $
      unit @Int 5

    after <- buildExpM $
      "l1" <=: store @Int 3        $
      "t"  <=: fetch "p" (Just 0)  $
      "a1" <=: fetch "p" (Just 1)  $
      "a2" <=: fetch "p" (Just 2)  $
      "a3" <=: fetch "p" (Just 3)  $
      "l2" <=: store @Int 4        $
      unit @Int 5

    splitFetch before `sameAs` after

  it "Example from Figure 4.14" $ do
    before <- buildExpM $
      "l1" <=: store @Int 3                                       $
      ("Int" #: ["x"]) <=: fetch "p" Nothing    $
      "l2" <=: store @Int 4                                       $
      unit @Int 5

    after <- buildExpM $
      "l1" <=: store @Int 3         $
      "x"  <=: fetch "p" (Just 1)   $
      "l2" <=: store @Int 4         $
      unit @Int 5

    splitFetch before `sameAs` after

  it "Example from Figure 4.15" $ do
    -- TODO Include hpt-result with t \elem { CPair }
    before <- buildExpM $
      "l1" <=: store @Int 3                                 $
      (VarTagNode "t" ["a1", "a2"]) <=: fetch "p" Nothing   $
      "l2" <=: store @Int 4                                 $
      unit @Int 5

    after <- buildExpM $
      "l1" <=: store @Int 3             $
      "t"  <=: unit @Val (tag' "Pair")  $
      "a1" <=: fetch "p" (Just 1)       $
      "a2" <=: fetch "p" (Just 2)       $
      "l2" <=: store @Int 4             $
      unit @Int 5

    splitFetch before `sameAs` after

  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "transformation has effect" $ property $
        forAll gen $ \before ->
          let after = splitFetch before
          in changed before after True


runTests :: IO ()
runTests = hspec spec
