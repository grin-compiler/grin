{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module ExtendedSyntax.ParserSpec where

import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck

import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Parse
import Grin.ExtendedSyntax.PrimOpsPrelude

import Test.ExtendedSyntax.Old.Test
import Test.ExtendedSyntax.Assertions

import Transformations.ExtendedSyntax.Conversion

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
    it "var-pat" $ do
      let before = [prog|
        grinMain =
          v <- pure ()
          pure v
        |]
      let after = Program []
            [ Def "grinMain" [] $
                EBind (SReturn Unit) (VarPat "v") (SReturn $ Var "v")
            ]
      before `sameAs` after

    it "as-pat-nullary-node" $ do
      let before = [prog|
        grinMain =
          (CNil)@v <- pure ()
          pure v
        |]
      let after = Program []
            [ Def "grinMain" [] $
                EBind (SReturn Unit) (AsPat (Tag C "Nil") [] "v") (SReturn $ Var "v")
            ]
      before `sameAs` after

    it "as-pat-node" $ do
      let before = [prog|
        grinMain =
          (CCons x xs)@v <- pure ()
          pure v
        |]
      let after = Program []
            [ Def "grinMain" [] $
                EBind (SReturn Unit) (AsPat (Tag C "Cons") ["x", "xs"] "v") (SReturn $ Var "v")
            ]
      before `sameAs` after

    it "case" $ do
      let before = [prog|
        test p =
          (CNil)@_cNil <- case p of
            #default @ _1 ->
              pure ()
          case p of
            #default @ _2 ->
              pure p
        |]
      let after = Program []
            [ Def "test"[ "p" ]
              ( EBind ( ECase "p" [ Alt DefaultPat "_1" ( SReturn Unit ) ] ) (AsPat (Tag C "Nil") [] "_cNil")
                ( ECase "p" [ Alt DefaultPat "_2" ( SReturn (Var "p") ) ] )
              )
            ]
      before `sameAs` after

    it "literal - bind" $ do
      let before = [prog|
        grinMain =
          x0 <- pure 13.1415
          x1 <- pure +13.1415
          x2 <- pure -13.1415
          x3 <- pure 42
          x4 <- pure +42
          x5 <- pure -42
          x6 <- pure 64u
          x7 <- pure #True
          x8 <- pure #False
          x9 <- pure ()
          pure (CNode x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
        |]
      let after = Program []
            [ Def "grinMain" [] $
                EBind ( SReturn ( Lit ( LFloat 13.1415 ) ) )    (VarPat "x0") $
                EBind ( SReturn ( Lit ( LFloat 13.1415 ) ) )    (VarPat "x1") $
                EBind ( SReturn ( Lit ( LFloat (-13.1415) ) ) ) (VarPat "x2") $
                EBind ( SReturn ( Lit ( LInt64 42 ) ) )         (VarPat "x3") $
                EBind ( SReturn ( Lit ( LInt64 42 ) ) )         (VarPat "x4") $
                EBind ( SReturn ( Lit ( LInt64 (-42) ) ) )      (VarPat "x5") $
                EBind ( SReturn ( Lit ( LWord64 64 ) ) )        (VarPat "x6") $
                EBind ( SReturn ( Lit ( LBool True ) ) )        (VarPat "x7") $
                EBind ( SReturn ( Lit ( LBool False ) ) )       (VarPat "x8") $
                EBind ( SReturn Unit )                          (VarPat "x9") $
                SReturn (ConstTagNode (Tag C "Node") ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"])


            ]
      before `sameAs` after

    it "literal - case" $ do
      let before = [prog|
        grinMain =
          case scrut of
            13.1415 @ _1   -> pure ()
            +14.1415 @ _2  -> pure ()
            -14.1415 @ _3  -> pure ()
            42 @ _4        -> pure ()
            +43 @ _5       -> pure ()
            -42 @ _6       -> pure ()
            64u @ _7       -> pure ()
            (CNode a1 a2 a3 a4 a5) @ _8 -> pure ()
            #default @ _9  -> pure ()
            #True @ _10    -> pure ()
            #False @ _11   -> pure ()
        |]
      let after = Program []
            [ Def "grinMain"[]
                ( ECase "scrut"
                    [ Alt ( LitPat ( LFloat 13.1415 ) ) "_1" ( SReturn Unit )
                    , Alt ( LitPat ( LFloat 14.1415 ) ) "_2" ( SReturn Unit )
                    , Alt ( LitPat ( LFloat ( -14.1415 ) ) ) "_3" ( SReturn Unit )
                    , Alt ( LitPat ( LInt64 42 ) ) "_4" ( SReturn Unit )
                    , Alt ( LitPat ( LInt64 43 ) ) "_5" ( SReturn Unit )
                    , Alt ( LitPat ( LInt64 ( -42 ) ) ) "_6" ( SReturn Unit )
                    , Alt ( LitPat ( LWord64 64 ) ) "_7" ( SReturn Unit )
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
                        ) "_8" ( SReturn Unit )
                    , Alt DefaultPat "_9" ( SReturn Unit )
                    , Alt ( LitPat ( LBool True ) ) "_10" ( SReturn Unit )
                    , Alt ( LitPat ( LBool False ) ) "_11" ( SReturn Unit )
                    ]
                )
            ]
      before `sameAs` after

    it "block" $ do
      let before = [prog|
        grinMain =
          a <- do
            pure ()
          pure ()
        |]
      let after = Program []
            [ Def "grinMain" [] $
                EBind (SBlock $ SReturn Unit) (VarPat "a") (SReturn Unit)
            ]
      before `sameAs` after

    it "nested block" $ do
      let before = [prog|
        grinMain =
          a <- do
            b <- do
              pure ()
            pure ()
          pure ()
        |]
      let after = Program []
            [ Def "grinMain" [] $
                EBind (SBlock $ EBind (SBlock $ SReturn Unit) (VarPat "b") (SReturn Unit)) (VarPat "a") (SReturn Unit)
            ]
      before `sameAs` after

    xit "bind case on left hand side" $ do
      let before = [expr|
          x <-
            case y of
              1 @ _1 -> pure 2
          pure x
      |]
      let after =
            EBind
              (ECase "y"
                [ Alt (LitPat (LInt64 1)) ("_1") $ SReturn $ Lit $ LInt64 2
                ])
              (VarPat "x") $
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
    pendingWith "store can only be applied to names in the new syntax"
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
    pendingWith "update can only be applied to names in the new syntax"
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
            v1 <- pure #""
            v2 <- pure #"a"
            v3 <- case v1 of
              #"" @ _1 -> pure 1
              #"a" @ _2 -> pure 2
              #default @ _3 -> pure 3
            pure ()
        |]
    let after = Program []
          [Def "grinMain" [] $
            EBind (SReturn (Lit (LString ""))) (VarPat "v1") $
            EBind (SReturn (Lit (LString "a"))) (VarPat "v2") $
            EBind (ECase "v1" $
              [Alt (LitPat (LString "")) "_1" (SReturn (Lit (LInt64 1)))
              ,Alt (LitPat (LString "a")) "_2" (SReturn (Lit (LInt64 2)))
              ,Alt DefaultPat "_3" (SReturn (Lit (LInt64 3)))
              ]) (VarPat "v3") $
            SReturn Unit
          ]
    before `sameAs` after

  it "char literals" $ do
    let before = [prog|
          grinMain =
            v2 <- pure #'a'
            v3 <- case v2 of
              #'b' @ _1 -> pure 1
              #'c' @ _2 -> pure 2
              #default @ _3 -> pure 3
            pure ()
        |]
    let after = Program []
          [Def "grinMain" [] $
            EBind (SReturn (Lit (LChar 'a'))) (VarPat "v2") $
            EBind (ECase "v2" $
              [Alt (LitPat (LChar 'b')) "_1" (SReturn (Lit (LInt64 1)))
              ,Alt (LitPat (LChar 'c')) "_2" (SReturn (Lit (LInt64 2)))
              ,Alt DefaultPat "_3" (SReturn (Lit (LInt64 3)))
              ]) (VarPat "v3") $
            SReturn Unit
          ]
    before `sameAs` after

  describe "external defintions" $ do
    it "primops-prelude" $ do
      let before = withPrimPrelude $ Program [] []
      let after  = primPrelude
      before `shouldBe` after

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

  -- TODO: Kind of hack for now. Now, we generate an old AST,
  -- convert it to the new syntax, then test for the property.
  -- We will need to fix Test.ExtendedSyntax.New.Test.
  describe "generated" $ do
    it "parse . pretty print == id" $ property $
      forAll (convertToNew <$> genProg) $ \newAst -> do
        let newAst' = either (error "Couldn't parse pretty printed AST, see generated NEW AST below.") id $
              parseGrin "" (Text.pack . show . WPP $ newAst)
        newAst' `sameAs` newAst
