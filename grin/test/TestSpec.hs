module TestSpec where

import Test
import Test.Hspec
import Test.QuickCheck

import Data.List (nub)

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "newNames generate unique names" $ property $
    forAll
      (do n <- choose (40, 50)
          runGoalUnsafe $ newNames n)
      (\ns -> length (nub ns) == length ns)
