{-# LANGUAGE QuasiQuotes #-}
module Transformations.ConfluenceSpec where

import Pipeline

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import GrinTH
import Assertions
import Data.List ( (\\) )

import Pretty (PP(..))
import Test (genProg)
import Transformations.MangleNames
import Control.Monad

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  let exp = [prog|
    grinMain =
      p1 <- store (CInt 0)
      p2 <- store (CInt 1)
      p3 <- store (CInt 1000)
      p4 <- store (Fupto p2 p3)
      p5 <- store (Fsum p1 p4)
      (Fsum p15 p16) <- fetch p5
      n13' <- sum p15 p16
      _prim_int_print n13'

    sum p10 p11 =
      (Fupto p17 p18) <- fetch p11
      p6 <- pure p17
      p7 <- pure p18
      (CInt n2') <- fetch p6
      (CInt n3') <- fetch p7
      b1' <- _prim_int_gt n2' n3'
      do
        case b1' of
          #True ->
            v10_1 <- pure (CNil)
            case v10_1 of
              (CNil) ->
                (CInt n14') <- fetch p10
                pure n14'
              (CCons p12 p13) ->
                (CInt n5') <- fetch p10
                (CInt n6') <- fetch p12
                n7' <- _prim_int_add n5' n6'
                p14 <- store (CInt n7')
                sum p14 p13
          #False ->
            n4' <- _prim_int_add n2' 1
            p8 <- store (CInt n4')
            p9 <- store (Fupto p8 p7)
            v10_2 <- pure (CCons p6 p9)
            case v10_2 of
              (CNil) ->
                (CInt n14'_2) <- fetch p10
                pure n14'_2
              (CCons p12_2 p13_2) ->
                (CInt n5'_2) <- fetch p10
                (CInt n6'_2) <- fetch p12_2
                n7'_2 <- _prim_int_add n5'_2 n6'_2
                p14_2 <- store (CInt n7'_2)
                sum p14_2 p13_2
  |]

  it "Random pipeline" $ do
    -- NOTE: This is a random test. This could make fail the build non-related to code changes.
    let opts = defaultOpts { _poLogging = False, _poOutputDir = "/tmp" }
    (pipeline1, transformed1) <- runPipeline opts exp randomPipeline
    (pipeline2, transformed2) <- runPipeline opts exp randomPipeline
    print pipeline1
    print pipeline2
    mangleNames transformed1 `sameAs` mangleNames transformed2

  -- Needs better code generation.
  xit "Random pipeline, random expression" $ property $
    forAll (PP <$> genProg) $ \(PP prog) -> monadicIO $ run $ do
      let opts = defaultOpts { _poLogging = True, _poOutputDir = "/tmp" }
      (pipeline1, transformed1) <- runPipeline opts prog randomPipeline
      (pipeline2, transformed2) <- runPipeline opts prog randomPipeline
      mangleNames transformed1 `sameAs` mangleNames transformed2
    
  it "Random optimization configuration" $ do
    let opts = defaultOpts { _poLogging = False, _poOutputDir = "/tmp" }
    property $
      forAllShrink ((,) <$> randomTransformations <*> randomTransformations) shrinkPermutations $ \(permutation1, permutation2) -> monadicIO $ do
        transformed1 <- run $ optimizeWith opts exp [] permutation1 []
        transformed2 <- run $ optimizeWith opts exp [] permutation2 []
        pure $ mangleNames transformed1 `sameAs` mangleNames transformed2

shrinkPermutations :: ([Transformation], [Transformation]) -> [([Transformation], [Transformation])]
shrinkPermutations ([x], [y]) = []
shrinkPermutations (xs, ys) = do
  x <- xs
  return (xs \\ [x], ys \\ [x])
