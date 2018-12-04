{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module ParserSpec where

import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck

import Grin.Pretty
import Grin.Grin
import Grin.TH
import Grin.Parse
import Test.Test
import Test.Assertions

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "quoted names" $ do
    it "basic" $ do
      let before = [prog|
        "GHC.Tuple.()" = pure (C"GHC.Tuple.()")
        |]
      let after = Program
            [ Def "GHC.Tuple.()" [] $ SReturn $ ConstTagNode (Tag C "GHC.Tuple.()") []
            ]
      before `sameAs` after

    it "special symbols" $ do
      let before = [prog|
        "extreme name with \" and ~ ! @ # $ % ^ & * ( ) | : > < > ? , . / " = pure ()
        |]
      let after = Program
            [ Def "extreme name with \" and ~ ! @ # $ % ^ & * ( ) | : > < > ? , . / " [] $ SReturn Unit
            ]
      before `sameAs` after

    it "parse . pretty == id" $ do
      let exp = Program
            [ Def "extreme name with \" and ~ ! @ # $ % ^ & * ( ) | : > < > ? , . / " [] $ SReturn Unit
            ]
      let Right parsedExp = parseGrin "" (Text.pack $ show $ PP exp)
      parsedExp `sameAs` exp

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

    it "literal - bind" $ do
      let before = [prog|
        grinMain =
          floatLit1 <- pure 13.1415
          nodeLit1 <- pure (CNode 13.1415 +13.1415 -13.1415 42 +42 -42 64u #True #False floatLit1)
          pure ()
        |]
      let after = Program
            [ Def "grinMain"[]
                ( EBind ( SReturn ( Lit ( LFloat 13.1415 ) ) ) ( Var "floatLit1" )
                    ( EBind
                        ( SReturn
                            ( ConstTagNode
                                ( Tag
                                    { tagType = C
                                    , tagName = "Node"
                                    }
                                )
                                [ Lit ( LFloat 13.1415 )
                                , Lit ( LFloat 13.1415 )
                                , Lit ( LFloat ( -13.1415 ) )
                                , Lit ( LInt64 42 )
                                , Lit ( LInt64 42 )
                                , Lit ( LInt64 ( -42 ) )
                                , Lit ( LWord64 64 )
                                , Lit ( LBool True )
                                , Lit ( LBool False )
                                , Var "floatLit1"
                                ]
                            )
                        ) ( Var "nodeLit1" ) ( SReturn Unit )
                    )
                )
            ]
      before `sameAs` after

    it "literal - case" $ do
      let before = [prog|
        grinMain =
          case -12.12 of
            13.1415   -> pure ()
            +14.1415  -> pure ()
            -14.1415  -> pure ()
            42        -> pure ()
            +43       -> pure ()
            -42       -> pure ()
            64u       -> pure ()
            (CNode a1 a2 a3 a4 a5) -> pure ()
            #default  -> pure ()
            #True     -> pure ()
            #False    -> pure ()
        |]
      let after = Program
            [ Def "grinMain"[]
                ( ECase ( Lit ( LFloat ( -12.12 ) ) )
                    [ Alt ( LitPat ( LFloat 13.1415 ) ) ( SReturn Unit )
                    , Alt ( LitPat ( LFloat 14.1415 ) ) ( SReturn Unit )
                    , Alt ( LitPat ( LFloat ( -14.1415 ) ) ) ( SReturn Unit )
                    , Alt ( LitPat ( LInt64 42 ) ) ( SReturn Unit )
                    , Alt ( LitPat ( LInt64 43 ) ) ( SReturn Unit )
                    , Alt ( LitPat ( LInt64 ( -42 ) ) ) ( SReturn Unit )
                    , Alt ( LitPat ( LWord64 64 ) ) ( SReturn Unit )
                    , Alt
                        ( NodePat
                            ( Tag
                                { tagType = C
                                , tagName = "Node"
                                }
                            )
                            [ "a1"
                            , "a2"
                            , "a3"
                            , "a4"
                            , "a5"
                            ]
                        ) ( SReturn Unit )
                    , Alt DefaultPat ( SReturn Unit )
                    , Alt ( LitPat ( LBool True ) ) ( SReturn Unit )
                    , Alt ( LitPat ( LBool False ) ) ( SReturn Unit )
                    ]
                )
            ]
      before `sameAs` after

    xit "bind case on left hand side" $ do
      let before = [expr|
          x <-
            case y of
              1 -> pure 2
          pure x
      |]
      let after =
            EBind
              (ECase (Var "y")
                [ Alt (LitPat (LInt64 1)) $ SReturn $ Lit $ LInt64 2
                ])
              (Var "x") $
            SReturn (Var "x")
      before `shouldBe` after

  describe "generated" $ do
    it "parse . pretty print == id" $ property $
      forAll (PP <$> genProg) $ \p ->
        let p' = parseGrin "" (Text.pack $ show p)
        in (fmap PP p') `shouldBe` (Right p)

