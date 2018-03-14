{-# LANGUAGE OverloadedStrings, LambdaCase, QuasiQuotes #-}
module Transformations.Simplifying.RightHoistFetchSpec where

import Control.Monad
import Test.Hspec
import Test hiding (asVal)
import Test.QuickCheck.Property

import Grin
import GrinTH
import Assertions
import Transformations.Simplifying.RightHoistFetch2

spec :: Spec
spec = do
  it "Example from Figure 4.16" $ do
    let before = [expr|
            l0 <- store 0
            p  <- store (CCons 1 2)
            t  <- fetch p[0]
            a1 <- fetch p[1]
            a2 <- fetch p[2]
            l1 <- store 1
            case t of
              CNil -> l3 <- store 2
                      unit 2
              CCons -> l4 <- store 3
                       l5 <- store a1
                       l6 <- store a2
                       pure 3
          |]
    let after = [expr|
            l0 <- store 0
            p  <- store (CCons 1 2)
            t  <- fetch p[0]
            l1 <- store 1
            case t of
              CNil -> l3 <- store 2
                      pure 2
              CCons -> a1a1 <- fetch p[1]
                       a2a1 <- fetch p[2]
                       l4 <- store 3
                       l5 <- store a1a1
                       l6 <- store a2a1
                       pure 3
          |]
    rightHoistFetch before `sameAs` after

  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "transformation has effect" $ property $
        forAll gen $ \before ->
          let after = rightHoistFetch before
          in changed before after True

runTests :: IO ()
runTests = hspec spec
