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
      let after = Program []
            [ Def "GHC.Tuple.()" [] $ SReturn $ ConstTagNode (Tag C "GHC.Tuple.()") []
            ]
      before `sameAs` after

    it "special symbols" $ do
      let before = [prog|
        "extreme name with \" and ~ ! @ # $ % ^ & * ( ) | : > < > ? , . / " = pure ()
        |]
      let after = Program []
            [ Def "extreme name with \" and ~ ! @ # $ % ^ & * ( ) | : > < > ? , . / " [] $ SReturn Unit
            ]
      before `sameAs` after

    it "parse . pretty == id" $ do
      let exp = Program []
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
      let after = Program []
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
      let after = Program []
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
      let after = Program []
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

  it "string literal" $ do
    let before = [prog|
          grinMain =
            v0 <- fun_call $ 1 #"a" 1
            v1 <- pure #""
            v2 <- pure #"a"
            v3 <- case #"" of
              #"" -> pure 1
              #"a" -> pure 2
              #default -> pure 3
            v4 <- pure (CTag1 #"")
            v5 <- pure (CTag2 #"" #"a")
            v6 <- pure (CTag3 #"a" #"")
            v7 <- pure (CTag4 #"a" #"" #"b")
            v8 <- pure (CTag5 1 #"" 1 #"a")
            v9 <- pure (CTag6 1 #"" 1 #"a" 4)
            v10 <- case (CTag7 1 #"" 3) of
              (CTag7 v11 v12 v13) -> pure 1
            v11 <- store (CTag8 #"a" 1 #"")
            (CTag9 #"a") <- pure (CTag9 #"a")
            pure ()
        |]
    let after = Program []
          [Def "grinMain" []
            (EBind (SApp "fun_call" [Lit (LInt64 1),Lit (LString "a"),Lit (LInt64 1)]) (Var "v0")
            (EBind (SReturn (Lit (LString ""))) (Var "v1")
            (EBind (SReturn (Lit (LString "a"))) (Var "v2")
            (EBind (ECase (Lit (LString ""))
              [Alt (LitPat (LString "")) (SReturn (Lit (LInt64 1)))
              ,Alt (LitPat (LString "a")) (SReturn (Lit (LInt64 2)))
              ,Alt DefaultPat (SReturn (Lit (LInt64 3)))
              ]) (Var "v3")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag1"}) [Lit (LString "")])) (Var "v4")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag2"}) [Lit (LString ""),Lit (LString "a")])) (Var "v5")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag3"}) [Lit (LString "a"),Lit (LString "")])) (Var "v6")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag4"}) [Lit (LString "a"),Lit (LString ""),Lit (LString "b")])) (Var "v7")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag5"}) [Lit (LInt64 1),Lit (LString ""),Lit (LInt64 1),Lit (LString "a")])) (Var "v8")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag6"}) [Lit (LInt64 1),Lit (LString ""),Lit (LInt64 1),Lit (LString "a"),Lit (LInt64 4)])) (Var "v9")
            (EBind (ECase (ConstTagNode (Tag {tagType = C, tagName = "Tag7"}) [Lit (LInt64 1),Lit (LString ""),Lit (LInt64 3)])
                      [Alt (NodePat (Tag {tagType = C, tagName = "Tag7"}) ["v11","v12","v13"]) (SReturn (Lit (LInt64 1)))]) (Var "v10")
            (EBind (SStore (ConstTagNode (Tag {tagType = C, tagName = "Tag8"}) [Lit (LString "a"),Lit (LInt64 1),Lit (LString "")])) (Var "v11")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag9"}) [Lit (LString "a")])) (ConstTagNode (Tag {tagType = C, tagName = "Tag9"}) [Lit (LString "a")])
            (SReturn Unit))))))))))))))
          ]
    before `shouldBe` after

  it "char literals" $ do
    let before = [prog|
          grinMain =
            v0 <- fun_call $ 1 #'a' 1
            v2 <- pure #'a'
            v3 <- case #'b' of
              #'b' -> pure 1
              #'c' -> pure 2
              #default -> pure 3
            v5 <- pure (CTag2 #'a' #'b')
            v6 <- pure (CTag3 #'a')
            v8 <- pure (CTag5 1 #'a')
            v9 <- pure (CTag6 1 #'a' 4)
            v10 <- case (CTag7 1 #'b' 3) of
              (CTag7 v11 v12 v13) -> pure 1
            v11 <- store (CTag8 #'a' 1 #'c')
            (CTag9 #'a') <- pure (CTag9 #'a')
            pure ()
        |]
    let after = Program []
          [Def "grinMain" []
            (EBind (SApp "fun_call" [Lit (LInt64 1),Lit (LChar 'a'),Lit (LInt64 1)]) (Var "v0")
            (EBind (SReturn (Lit (LChar 'a'))) (Var "v2")
            (EBind (ECase (Lit (LChar 'b'))
              [Alt (LitPat (LChar 'b')) (SReturn (Lit (LInt64 1)))
              ,Alt (LitPat (LChar 'c')) (SReturn (Lit (LInt64 2)))
              ,Alt DefaultPat (SReturn (Lit (LInt64 3)))
              ]) (Var "v3")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag2"}) [Lit (LChar 'a'),Lit (LChar 'b')])) (Var "v5")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag3"}) [Lit (LChar 'a')])) (Var "v6")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag5"}) [Lit (LInt64 1),Lit (LChar 'a')])) (Var "v8")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag6"}) [Lit (LInt64 1),Lit (LChar 'a'),Lit (LInt64 4)])) (Var "v9")
            (EBind (ECase (ConstTagNode (Tag {tagType = C, tagName = "Tag7"}) [Lit (LInt64 1),Lit (LChar 'b'),Lit (LInt64 3)])
              [Alt (NodePat (Tag {tagType = C, tagName = "Tag7"}) ["v11","v12","v13"]) (SReturn (Lit (LInt64 1)))
              ]) (Var "v10")
            (EBind (SStore (ConstTagNode (Tag {tagType = C, tagName = "Tag8"}) [Lit (LChar 'a'),Lit (LInt64 1),Lit (LChar 'c')])) (Var "v11")
            (EBind (SReturn (ConstTagNode (Tag {tagType = C, tagName = "Tag9"}) [Lit (LChar 'a')])) (ConstTagNode (Tag {tagType = C, tagName = "Tag9"}) [Lit (LChar 'a')])
            (SReturn Unit)))))))))))
          ]
    before `shouldBe` after

  describe "external defintions" $ do
    it "primop" $ do
      let before = [prog|
        primop effectful
          _prim_string_print  :: T_String -> T_Unit
          _prim_read_string   :: T_String

          "newArrayArray#" :: {"Int#"} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArrayArray#" %s}}

        primop pure
          _prim_string_concat   :: T_String -> T_String -> T_String

        ffi pure
          newArrayArray :: {Int} -> {State %s} -> {GHC.Prim.Unit {MutableArrayArray %s}}

        grinMain = pure ()
        |]
      let after = Program

            [ External
                { eName = "_prim_string_print"
                , eRetType = TySimple T_Unit
                , eArgsType = [ TySimple T_String ]
                , eEffectful = True
                , eKind = PrimOp
                }
            , External
                { eName = "_prim_read_string"
                , eRetType = TySimple T_String
                , eArgsType = []
                , eEffectful = True
                , eKind = PrimOp
                }
            , External
                { eName = "newArrayArray#"
                , eRetType = TyCon "GHC.Prim.Unit#"
                    [ TyCon "MutableArrayArray#" [ TyVar "s" ] ]
                , eArgsType =
                    [ TyCon "Int#" []
                    , TyCon "State#" [ TyVar "s" ]
                    ]
                , eEffectful = True
                , eKind = PrimOp
                }
            , External
                { eName = "_prim_string_concat"
                , eRetType = TySimple T_String
                , eArgsType =
                    [ TySimple T_String
                    , TySimple T_String
                    ]
                , eEffectful = False
                , eKind = PrimOp
                }
            , External
                { eName = "newArrayArray"
                , eRetType = TyCon "GHC.Prim.Unit"
                    [ TyCon "MutableArrayArray" [ TyVar "s" ] ]
                , eArgsType =
                    [ TyCon "Int" []
                    , TyCon "State" [ TyVar "s" ]
                    ]
                , eEffectful = False
                , eKind = FFI
                }
            ]
            [ Def "grinMain" [] ( SReturn Unit ) ]
      before `sameAs` after

    it "indentation" $ do
      let before = [prog|
        primop pure
          -- comment
          _primA :: T_String
                 -> T_String
                 -> T_String
          {-
            comment
          -}
          _primB
           :: T_String
           -> T_String

          -- comment
          {-
            comment
          -}
        |]
      let after =
            Program
              [ External
                  { eName = NM { unNM = "_primA" }
                  , eRetType = TySimple T_String
                  , eArgsType =
                      [ TySimple T_String
                      , TySimple T_String
                      ]
                  , eEffectful = False
                  , eKind = PrimOp
                  }
              , External
                  { eName = NM { unNM = "_primB" }
                  , eRetType = TySimple T_String
                  , eArgsType = [ TySimple T_String ]
                  , eEffectful = False
                  , eKind = PrimOp
                  }
              ] []

      before `sameAs` after

  describe "generated" $ do
    it "parse . pretty print == id" $ property $
      forAll (PP <$> genProg) $ \p ->
        let p' = parseGrin "" (Text.pack $ show p)
        in (fmap PP p') `shouldBe` (Right p)
