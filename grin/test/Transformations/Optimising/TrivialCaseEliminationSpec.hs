{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.TrivialCaseEliminationSpec where

import Transformations.Optimising.TrivialCaseElimination

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
    it "Figure 4.24" $ do
      let before = [expr|
          case v of
            (Ffun a1 a2 a3) -> fun a1 a2 a3
        |]
      let after = [expr|
          (Ffun a1 a2 a3) <- pure v
          fun a1 a2 a3
        |]
      trivialCaseElimination (ctx before) `sameAs` (ctx after)
