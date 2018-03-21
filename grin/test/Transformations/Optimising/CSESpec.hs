{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.CSESpec where

import Transformations.Optimising.CSE

import Test.Hspec
import Grin
import GrinTH
import Test hiding (newVar)
import Assertions
import ParseGrin
import TypeEnv
import Data.Monoid
import Control.Arrow


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  testExprContext $ \ctx -> do
    it "Figure 4.34" $ do
      let teBefore = create $
            (newVar "x'" int64_t)

      let before = [expr|
          y' <- intAdd x' 1
          t' <- pure CInt
          p <- store (t' y')
          fun 1 2
          z' <- intAdd x' 1
          r' <- pure CInt
          q <- store (r' z')
          fun 2 3
          (CInt a') <- fetch p
          (CInt b') <- fetch q
          fun 3 4
        |]
      let after = [expr|
          y' <- intAdd x' 1
          t' <- pure CInt
          p <- store (t' y')
          fun 1 2
          z' <- pure y'
          r' <- pure t'
          q <- store (r' z')
          fun 2 3
          (CInt a') <- pure (t' y')
          (CInt b') <- pure (r' z')
          fun 3 4
        |]
      commonSubExpressionElimination (ctx (teBefore, before)) `sameAs` (ctx (teBefore, after))
