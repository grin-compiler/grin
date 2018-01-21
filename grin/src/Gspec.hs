module Gspec where

import Control.Monad (zipWithM_)
import Test.QuickCheck hiding (generate, sample)
import Test.QuickCheck.Gen hiding (sample)
import Test.QuickCheck.Random
import Test.QuickCheck.Arbitrary
import Test.Hspec


sample :: Int -> Gen a -> [a]
sample m g = [ gen n (n * n) g | n <- [0 .. m] ]
  where
    gen :: Int -> Int -> Gen a -> a
    gen p s (MkGen g) = g (mkQCGen p) s

generate :: Int -> Gen a -> (a -> Spec) -> Spec
generate m gen test = do
  sequence_ $ zipWith
    (\n x -> describe ("#" ++ show n) (test x))
    [1..]
    (sample m gen)
