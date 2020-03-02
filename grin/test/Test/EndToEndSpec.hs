{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Test.EndToEndSpec where

import Control.Monad.Reader
import Test.Hspec
import Test.EndToEnd
import Test.QuickCheck hiding (Failure)
import Data.ByteString.Char8
import Test.Hspec.Core.Spec hiding (pending)
import Data.Map as Map


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Finds failing element in the middle." $ property $
    -- Assumption: Min passes, Mid, Max fails, min < mid, mid <= max
    forAll (choose (0,100)) $ \mn ->
    forAll (choose (mn+1, mn+101)) $ \mx ->
    forAll (choose (mn+1, mx)) $ \md -> do
      let res = testBisectM (mn,md,mx) $ bisect "some-dir" "some-result"
      show res `shouldBe` (show $ Result "" $ Failure Nothing $ Reason $ "Test failed in pipeline step: " ++ show md)

newtype TestBisectM a = TBM (Reader (Int, Int, Int) a)
  deriving (Functor, Applicative, Monad, MonadReader (Int, Int, Int))

testBisectM :: (Int, Int, Int) -> TestBisectM a -> a
testBisectM e (TBM m) = runReader m e

instance BisectM TestBisectM where
  createFileMap _ = asks (\(mn,_,mx) -> Map.fromList [ (n, show n) | n <- [mn..mx] ])
  runTest n _     = asks (\(_,md,_) -> read n < md)
