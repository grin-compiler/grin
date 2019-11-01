{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes #-}
module AbstractInterpretation.ExtendedSyntax.EffectTrackingSpec where

import Data.Map (Map)

import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.PrimOpsPrelude

import Test.Hspec
import Test.ExtendedSyntax.Assertions

import AbstractInterpretation.ExtendedSyntax.Reduce (AbstractInterpretationResult(..),evalAbstractProgram)
import AbstractInterpretation.ExtendedSyntax.EffectTracking.CodeGen hiding (live)
import AbstractInterpretation.ExtendedSyntax.EffectTracking.Result


runTests :: IO ()
runTests = hspec spec

calcEffects :: Exp -> ETResult
calcEffects prog
  | (etProgram, etMapping) <- codeGen (withPrimPrelude prog)
  , computer <- _airComp . evalAbstractProgram $ etProgram
  = toETResult etMapping computer

calcEffectsWithoutPrimopsPrelude :: Exp -> ETResult
calcEffectsWithoutPrimopsPrelude prog
  | (etProgram, etMapping) <- codeGen prog
  , computer <- _airComp . evalAbstractProgram $ etProgram
  = toETResult etMapping computer

spec :: Spec
spec = describe "Effect Tracking Analysis" $ do

  it "simple_print" $ do
    let exp = [prog|
          grinMain =
            zero <- pure 0
            _prim_int_print zero
        |]
    let expected = mempty
          { _register = [ ("zero", Effects []) ]
          , _function = [ ("grinMain", Effects ["_prim_int_print"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bound_print" $ do
    let exp = [prog|
          grinMain =
            zero <- pure 0
            x <- _prim_int_print zero
            pure x
        |]
    let expected = mempty
          { _register = [ ("zero", Effects [])
                        , ("x", Effects ["_prim_int_print"])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_int_print"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "fun_print" $ do
    let exp = [prog|
          grinMain =
            zero <- pure 0
            f zero

          f x = _prim_int_print x
        |]
    let expected = mempty
          { _register = [ ("zero", Effects []) ]
          , _function = [ ("f", Effects ["_prim_int_print"])
                        , ("grinMain", Effects ["_prim_int_print"])
                        ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bound_fun_print" $ do
    let exp = [prog|
          grinMain =
            zero <- pure 0
            y <- f zero
            pure y

          f x = _prim_int_print x
        |]
    let expected = mempty
          { _register = [ ("zero", Effects [])
                        , ("y", Effects ["_prim_int_print"])
                        ]
          , _function = [ ("f", Effects ["_prim_int_print"])
                        , ("grinMain", Effects ["_prim_int_print"])
                        ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bound_in_fun_print" $ do
    let exp = [prog|
          grinMain =
            zero <- pure 0
            z <- f zero
            pure z

          f x =
            y <- _prim_int_print x
            pure y
        |]
    let expected = mempty
          { _register = [ ("zero", Effects [])
                        , ("y", Effects ["_prim_int_print"])
                        , ("z", Effects ["_prim_int_print"])
                        ]
          , _function = [ ("f", Effects ["_prim_int_print"])
                        , ("grinMain", Effects ["_prim_int_print"])
                        ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  -- NOTE: overapproximation
  it "simple_case" $ do
    let exp = [prog|
          grinMain =
            n <- pure (COne)
            y <- case n of
              (COne)@_1 ->
                zero <- pure 0
                _prim_int_print zero
              (CTwo)@_2 ->
                asd <- pure #"asd"
                _prim_string_print asd
              (CFoo)@_3 ->
                pure ()
            pure y
        |]
    let expected = mempty
          { _register = [ ("zero", Effects [])
                        , ("asd", Effects [])
                        , ("y", Effects ["_prim_int_print", "_prim_string_print"])
                        , ("n", Effects [])

                        , ("_1", Effects [])
                        , ("_2", Effects [])
                        , ("_3", Effects [])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_int_print", "_prim_string_print"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "fun_case" $ do
    let exp = [prog|
          grinMain =
            n <- pure (COne)
            y <- case n of
              (COne)@_1 ->
                zero <- pure 0
                f zero
              (CTwo)@_2 ->
                asd <- pure #"asd"
                g asd
              (CFoo)@_3 -> h
            pure y

          f x1 = _prim_int_print x1
          g x2 = _prim_string_print x2
          h    = pure ()
        |]
    let expected = mempty
          { _register = [ ("zero", Effects [])
                        , ("asd", Effects [])
                        , ("y", Effects ["_prim_int_print", "_prim_string_print"])
                        , ("n", Effects [])

                        , ("_1", Effects [])
                        , ("_2", Effects [])
                        , ("_3", Effects [])
                        ]
          , _function = [ ("f",  Effects ["_prim_int_print"])
                        , ("g",  Effects ["_prim_string_print"])
                        , ("h",  Effects [])
                        , ("grinMain", Effects ["_prim_int_print", "_prim_string_print"])
                        ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "case_in_case" $ do
    let exp = [prog|
          grinMain =
            n <- pure (COne)
            y <- case n of
              (COne)@_1 ->
                z <- case n of
                  (COne)@_11 ->
                    zero <- pure 0
                    _prim_int_print zero
                  (CTwo)@_12 ->
                    errMsg <- pure "Never should have come here"
                    _prim_error errMsg
                pure z
              (CTwo)@_2 ->
                asd <- pure #"asd"
                _prim_string_print asd
              (CFoo)@_3 -> pure ()
            pure y
        |]
    let expected = mempty
          { _register = [ ("zero", Effects [])
                        , ("asd", Effects [])
                        , ("errMsg", Effects [])
                        , ("y", Effects ["_prim_int_print", "_prim_string_print", "_prim_error"])
                        , ("z", Effects ["_prim_int_print", "_prim_error"])
                        , ("n", Effects [])

                        , ("_1",  Effects [])
                        , ("_11", Effects [])
                        , ("_12", Effects [])
                        , ("_2",  Effects [])
                        , ("_3",  Effects [])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_int_print", "_prim_string_print", "_prim_error"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bind_sequence" $ do
    let exp = [prog|
          grinMain =
            zero <- pure 0
            asd <- pure #"asd"
            x1 <- pure (COne)
            x2 <- _prim_int_print zero
            x3 <- pure zero
            x4 <- _prim_int_add zero zero
            x5 <- _prim_string_print asd
            pure ()
        |]
    let expected = mempty
          { _register = [ ("zero", Effects [])
                        , ("asd", Effects [])
                        , ("x1", Effects [])
                        , ("x2", Effects ["_prim_int_print"])
                        , ("x3", Effects [])
                        , ("x4", Effects [])
                        , ("x5", Effects ["_prim_string_print"])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_int_print", "_prim_string_print"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bind_sequence_in_fun" $ do
    let exp = [prog|
          grinMain =
            k <- pure 0
            y1 <- f k
            y2 <- pure (CInt k)
            y3 <- g k
            pure ()

          f n =
            asd <- pure #"asd"
            x1 <- pure (COne)
            x2 <- _prim_int_print n
            x3 <- pure n
            x4 <- _prim_int_add n n
            x5 <- _prim_string_print asd
            pure ()

          g m =
            errMsg <- pure "ERROR"
            z1 <- pure (CTwo)
            z2 <- pure n
            z3 <- _prim_error errMsg
            z4 <- _prim_int_add n n
            pure ()
        |]
    let expected = mempty
          { _register = [ ("k", Effects [])
                        , ("asd", Effects [])
                        , ("errMsg", Effects [])
                        , ("x1", Effects [])
                        , ("x2", Effects ["_prim_int_print"])
                        , ("x3", Effects [])
                        , ("x4", Effects [])
                        , ("x5", Effects ["_prim_string_print"])
                        , ("y1", Effects ["_prim_int_print", "_prim_string_print"])
                        , ("y2", Effects [])
                        , ("y3", Effects ["_prim_error"])
                        , ("z1", Effects [])
                        , ("z2", Effects [])
                        , ("z3", Effects ["_prim_error"])
                        , ("z4", Effects [])
                        ]
          , _function = [ ("f", Effects ["_prim_int_print", "_prim_string_print"])
                        , ("g", Effects ["_prim_error"])
                        , ("grinMain", Effects ["_prim_int_print", "_prim_string_print", "_prim_error"])
                        ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "custom_externals" $ do
    let exp = [prog|
          primop effectful
            _prim_string_print  :: T_String -> T_Unit
            _prim_read_string   :: T_String

            "newPrimSomething#" :: {"GHC.Prim.SomePrimType#"}

          primop pure
            _prim_string_concat   :: T_String -> T_String -> T_String

          ffi pure
            newPrimSomething :: {GHC.Prim.SomePrimType}

          grinMain =
            helloWorld <- pure "Hello World!"
            hello <- pure "Hello"
            world <- pure "World"
            x0 <- _prim_string_print helloWorld
            x1 <- _prim_read_string
            x2 <- "newPrimSomething#" $
            x3 <- _prim_string_concat hello world
            x4 <- newPrimSomething
            pure ()
        |]
    let expected = mempty
          { _register = [ ("helloWorld", Effects [])
                        , ("hello", Effects [])
                        , ("world", Effects [])
                        , ("x0", Effects ["_prim_string_print"])
                        , ("x1", Effects ["_prim_read_string"])
                        , ("x2", Effects ["newPrimSomething#"])
                        , ("x3", Effects [])
                        , ("x4", Effects [])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_string_print", "_prim_read_string", "newPrimSomething#"]) ]
          }
        calculated = (calcEffectsWithoutPrimopsPrelude exp) { _external = mempty }
    calculated `sameAs` expected

  it "mixed_externals" $ do
    let exp = [prog|
          primop effectful
            "newPrimSomething#" :: {"GHC.Prim.SomePrimType#"}

          ffi pure
            newPrimSomething :: {GHC.Prim.SomePrimType}

          grinMain =
            helloWorld <- pure "Hello World!"
            hello <- pure "Hello"
            world <- pure "World"
            x0 <- _prim_string_print helloWorld
            x1 <- _prim_read_string
            x2 <- "newPrimSomething#" $
            x3 <- _prim_string_concat hello world
            x4 <- newPrimSomething
            pure ()
        |]
    let expected = mempty
          { _register = [ ("helloWorld", Effects [])
                        , ("hello", Effects [])
                        , ("world", Effects [])
                        , ("x0", Effects ["_prim_string_print"])
                        , ("x1", Effects ["_prim_read_string"])
                        , ("x2", Effects ["newPrimSomething#"])
                        , ("x3", Effects [])
                        , ("x4", Effects [])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_string_print", "_prim_read_string", "newPrimSomething#"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected


  it "redefining an already existing external is not allowed" $ do
    let exp = [prog|
          primop effectful
            -- already defined in PrimopsPrelude
            _prim_string_print  :: T_String -> T_Unit

          grinMain =
            helloWorld <- "Hello World!"
            x0 <- _prim_string_print helloWorld
            pure x
        |]
    let expected = mempty
          { _register = [ ("helloWorld", Effects [])
                        , ("x0", Effects ["_prim_string_print"])
                        , ("x1", Effects ["_prim_read_string"])
                        , ("x2", Effects ["newPrimSomething#"])
                        , ("x3", Effects [])
                        , ("x4", Effects [])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_string_print", "_prim_read_string", "newPrimSomething#"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    (return $! calculated) `shouldThrow` anyException
