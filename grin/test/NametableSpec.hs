module NametableSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Test
import Grin.Nametable
import Grin.Pretty


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Property" $ do
    it "restore . convert == id" $ property $
      forAll genProg $ \p ->
        let p' = restore $ convert p
        in (PP p') `shouldBe` (PP p)
