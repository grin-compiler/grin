{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Pretty
import Grin
import GrinTH
import ParseGrin
import Test
import Assertions

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "simple" $ do
    it "case" $ do
      let before = [prog|
        test p =
          case p of
            #default ->
              pure ()
          case p of
            #default ->
              pure p
        |]
      let after = Program
            [ Def "test"[ "p" ]
              ( EBind ( ECase ( Var "p" ) [ Alt DefaultPat ( SReturn Unit ) ] ) Unit
                ( ECase ( Var "p" ) [ Alt DefaultPat ( SReturn ( Var "p" ) ) ] )
              )
            ]
      before `sameAs` after

  describe "generated" $ do
    it "parse . pretty print == id" $ property $
      forAll (PP <$> genProg) $ \p ->
        let p' = parseGrin "" (show p)
        in (fmap PP p') `shouldBe` (Right p)
