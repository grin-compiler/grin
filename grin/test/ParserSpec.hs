module ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Pretty
import ParseGrin
import Test

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "parse . pretty print == id" $ property $
    forAll (PP <$> genProg) $ \p ->
      let p' = parseGrin "" (show p)
      in (fmap PP p') `shouldBe` (Right p)
