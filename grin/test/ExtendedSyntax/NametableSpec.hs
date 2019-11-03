module ExtendedSyntax.NametableSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Test.ExtendedSyntax.Old.Test
import Grin.ExtendedSyntax.Nametable
import Grin.ExtendedSyntax.Pretty
import Transformations.ExtendedSyntax.Conversion (convertToNew)


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Property" $ do
    it "restore . convert == id" $ property $
      forAll (convertToNew <$> genProg) $ \p ->
        let p' = restore $ convert p
        in (PP p') `shouldBe` (PP p)
