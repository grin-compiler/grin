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

  it "interleaved typeenv" $ do
    let exp = [text|
            % grinMain :: T_Int64
            grinMain =
              % a -> T_Int64
              a <- pure 5
              % n -> {CInt[T_Int64]}
              n <- pure (CInt a)
              % 0 -> {CInt[T_Int64]}
              % p -> {0}
              p <- store n
              pure 5
        |]
    let env = parseMarkedTypeEnv' exp
    env `sameAs` (parseTypeEnv . Text.pack . show . WPP $ env)

  it "pure undefined ast" $ do
    let exp = [prog|
            grinMain =
              x0 <- pure (#undefined :: T_Int64)
              x1 <- pure (#undefined :: T_Word64)
              x4 <- pure (#undefined :: T_Float)
              x2 <- pure (#undefined :: T_Bool)
              x3 <- pure (#undefined :: T_Unit)
              p0 <- pure (#undefined :: #ptr)
              p1 <- pure (#undefined :: {0})
              p2 <- pure (#undefined :: {0,1})
              n0 <- pure (#undefined :: {CInt[T_Int64]})
              n1 <- pure (#undefined :: {CPair[T_Int64, T_Bool]})
              n2 <- pure (#undefined :: {CPair[T_Int64, {0}]})
              n3 <- pure (#undefined :: {CPair[T_Int64, {0,1}]})
              n4 <- pure (#undefined :: {CPair[T_Int64, #ptr]})
              n5 <- pure (#undefined :: {CTriplet[T_Int64, {0,1}, #ptr]})
              pure 0
        |]
    exp `sameAs` (parseProg . Text.pack . show . WPP $ exp)

  it "store undefined" $ do
    let exp = [prog|
            grinMain =
              p0 <- store (#undefined :: {CInt[T_Int64]})
              p1 <- store (#undefined :: {CPair[T_Int64, T_Bool]})
              p2 <- store (#undefined :: {CPair[T_Int64, {0}]})
              p3 <- store (#undefined :: {CPair[T_Int64, {0,1}]})
              p4 <- store (#undefined :: {CPair[T_Int64, #ptr]})
              p5 <- store (#undefined :: {CTriplet[T_Int64, {0,1}, #ptr]})
              pure 0
          |]
    exp `sameAs` (parseProg . Text.pack . show . WPP $ exp)

  it "update undefined" $ do
    let exp = [prog|
            grinMain p =
              update p (#undefined :: {CInt[T_Int64]})
              update p (#undefined :: {CPair[T_Int64, T_Bool]})
              update p (#undefined :: {CPair[T_Int64, {0}]})
              update p (#undefined :: {CPair[T_Int64, {0,1}]})
              update p (#undefined :: {CPair[T_Int64, #ptr]})
              update p (#undefined :: {CTriplet[T_Int64, {0,1}, #ptr]})
              pure 0
        |]
    exp `sameAs` (parseProg . Text.pack . show . WPP $ exp)

  describe "generated" $ do
    it "parse . pretty print == id" $ property $
      forAll (PP <$> genProg) $ \p ->
        let p' = parseGrin "" (Text.pack $ show p)
        in (fmap PP p') `shouldBe` (Right p)

