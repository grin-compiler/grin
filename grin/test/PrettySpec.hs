{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrettySpec where

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

  describe "external defintions" $ do
    it "primop + ffi" $ do
      let sample = [prog|
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
      let before = parseGrin "" (Text.pack . show $ (PP $ prettyProgram WithExternals sample))
          after = Program

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
      (fmap PP before) `shouldBe` (Right $ PP after)
