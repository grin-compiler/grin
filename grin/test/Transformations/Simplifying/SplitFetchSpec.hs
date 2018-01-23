{-# LANGUAGE TypeApplications, OverloadedStrings, LambdaCase #-}

module Transformations.Simplifying.SplitFetchSpec where

import Test.Hspec

import Free
import Grin
import Test
import Transformations.Simplifying.SplitFetch


spec :: Spec
spec = do
  it "Example from Figure 4.13" $ do
    before <- buildExpM $
      Unit <=: store @Int 3                                       $
      (VarTagNode "t" ["a1", "a2", "a3"]) <=: fetch "p" Nothing   $
      Unit <=: store @Int 4                                       $
      unit @Int 5

    after <- buildExpM $
      Unit <=: store @Int 3        $
      "t"  <=: fetch "p" (Just 0)  $
      "a1" <=: fetch "p" (Just 1)  $
      "a2" <=: fetch "p" (Just 2)  $
      "a3" <=: fetch "p" (Just 3)  $
      Unit <=: store @Int 4        $
      unit @Int 5

    splitFetch before `shouldBe` after

  it "Example from Figure 4.14" $ do
    before <- buildExpM $
      Unit <=: store @Int 3                                       $
      ("Int" #: ["x"]) <=: fetch "p" Nothing    $
      Unit <=: store @Int 4                                       $
      unit @Int 5

    after <- buildExpM $
      Unit <=: store @Int 3         $
      "x"  <=: fetch "p" (Just 1)   $
      Unit <=: store @Int 4         $
      unit @Int 5

    splitFetch before `shouldBe` after

  it "Example from Figure 4.15" $ do
    -- TODO Include hpt-result with t \elem { CPair }
    before <- buildExpM $
      Unit <=: store @Int 3                                 $
      (VarTagNode "t" ["a1", "a2"]) <=: fetch "p" Nothing   $
      Unit <=: store @Int 4                                 $
      unit @Int 5

    after <- buildExpM $
      Unit <=: store @Int 3             $
      "t"  <=: unit @Val (tag' "Pair")  $
      "a1" <=: fetch "p" (Just 1)       $
      "a2" <=: fetch "p" (Just 2)       $
      Unit <=: store @Int 4             $
      unit @Int 5

    splitFetch before `shouldBe` after


runTest :: IO ()
runTest = hspec spec
