{-# LANGUAGE QuasiQuotes #-}
module Transformations.ExtendedSyntax.ConfluenceSpec where

import Pipeline.ExtendedSyntax.Pipeline

import Control.Monad
import Data.List ( (\\) )
import System.Random

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.PrimOpsPrelude
import Test.ExtendedSyntax.Assertions

import Grin.ExtendedSyntax.Pretty (PP(..))
-- TODO: replace with Test.ExtendedSyntax.New.Test
import Test.ExtendedSyntax.Old.Test (genProg)
import Transformations.ExtendedSyntax.MangleNames

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  let exp = withPrimPrelude [prog|
    grinMain =
      y.0 <- pure 1
      v.0 <- pure (CInt y.0)
      t1  <- store v.0
      y.1 <- pure 10000
      v.1 <- pure (CInt y.1)
      t2  <- store v.1
      v.2 <- pure (Fupto t1 t2)
      t3  <- store v.2
      v.3 <- pure (Fsum t3)
      t4  <- store v.3
      (CInt r') @ p.0 <- eval $ t4
      _prim_int_print $ r'

    upto m n =
      (CInt m') @ p.2 <- eval $ m
      (CInt n') @ p.1 <- eval $ n
      b' <- _prim_int_gt $ m' n'
      case b' of
        #True @ alt.0 ->
          v.4 <- pure (CNil)
          pure v.4
        #False @ alt.1 ->
          x.7 <- pure 1
          m1' <- _prim_int_add $ m' x.7
          v.5 <- pure (CInt m1')
          m1  <- store v.5
          v.6 <- pure (Fupto m1 n)
          p   <- store v.6
          v.7 <- pure (CCons m p)
          pure v.7

    sum l =
      l2 <- eval $ l
      case l2 of
        (CNil) @ alt.2 ->
          y.10 <- pure 0
          v.8  <- pure (CInt y.10)
          pure v.8
        (CCons x xs) @ alt.3 ->
          (CInt x') @ p.4 <- eval $ x
          (CInt s') @ p.3 <- sum $ xs
          ax' <- _prim_int_add $ x' s'
          v.9 <- pure (CInt ax')
          pure v.9

    eval q =
      v <- fetch q
      case v of
        (CInt x'1) @ alt.4 ->
          pure v
        (CNil) @ alt.5 ->
          pure v
        (CCons y ys) @ alt.6 ->
          pure v
        (Fupto a b) @ alt.7 ->
          w <- upto $ a b
          p.5 <- update q w
          pure w
        (Fsum c) @ alt.8 ->
          z <- sum $ c
          p.6 <- update q z
          pure z
  |]

  it "Random pipeline" $ do
    -- NOTE: This is a random test. This could make fail the build non-related to code changes.
    let opts = defaultOpts { _poLogging = False, _poOutputDir = "/tmp" }
    forAll arbitrary $ \(seed1, seed2) -> monadicIO $ run $ do
      transformed1 <- randomPipeline (mkStdGen seed1) opts exp
      transformed2 <- randomPipeline (mkStdGen seed2) opts exp
      mangleNames transformed1 `sameAs` mangleNames transformed2

  -- TODO: replace with code generation guided by the new syntax
  -- Needs better code generation.
  xit "Random pipeline, random expression" $ property $
    forAll (PP <$> genProg) $ \(PP prog) -> monadicIO $ run $ do
      let opts = defaultOpts { _poLogging = False, _poOutputDir = "/tmp" }
      transformed1 <- randomPipeline (mkStdGen 0xffaa419371) opts exp
      transformed2 <- randomPipeline (mkStdGen 0x51437291fb) opts exp
      mangleNames transformed1 `sameAs` mangleNames transformed2
