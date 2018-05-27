{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.DeadParameterEliminationSpec where

import Transformations.Optimising.DeadParameterElimination

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
          funA a b = pure b
          funB c = funA c 1
      |]
    let after = [prog|
          funA b = pure b
          funB c = funA 1
      |]
    deadParameterElimination before `sameAs` after
