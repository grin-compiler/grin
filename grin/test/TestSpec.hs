module TestSpec where

import Test.ExtendedSyntax.Old.Test
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.DeepSeq
import Test.Check

import Data.List (nub)
import qualified Data.Set as Set


runTests :: IO ()
runTests = hspec spec

uniqueValues :: (Eq a) => [a] -> Property
uniqueValues xs = property $ length (nub xs) == length xs

spec :: Spec
spec = do
  it "newNames generate unique names" $ property $
    forAll
      (do n <- choose (40, 50)
          runGoalUnsafe $ newNames n)
      uniqueValues

  it "withGADTs generate unique tags as constructors" $ do
    pendingWith "commented out due type error"
    {-
    property $ forAll
      (do n <- abs <$> arbitrary
          runGoalUnsafe $ withADTs n getADTs)
      (uniqueValues . concatMap tagNames . Set.toList)
    -}

  it "genProg does not generate big programs" $ property $
    forAll genProg $ \p -> label (show $ programSize p) $
      monadicIO $ do
        () <- pure $ rnf p
        pure ()
