{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.UpdateEliminationSpec where

import Transformations.Optimising.UpdateElimination

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
  testExprContextE $ \ctx -> do
    it "Figure 4.27" $ do
      let before = [expr|
          v <- fetch p
          update p v
          pure 1
        |]
      let after = [expr|
          v <- fetch p
          pure 1
        |]
      updateElimination (ctx before) `sameAs` (ctx after)
