{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes #-}
module AbstractInterpretation.EffectTrackingSpec where

import Data.Map (Map)

import Grin.TH
import Grin.Grin
import Grin.PrimOpsPrelude

import Test.Hspec
import Test.Assertions

import AbstractInterpretation.Reduce (AbstractInterpretationResult(..),evalAbstractProgram)
import AbstractInterpretation.EffectTracking.CodeGen hiding (live)
import AbstractInterpretation.EffectTracking.Result

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
            _prim_int_print 0
        |]
    let expected = mempty
          { _register = []
          , _function = [ ("grinMain", Effects ["_prim_int_print"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bound_print" $ do
    let exp = [prog|
          grinMain =
            x <- _prim_int_print 0
            pure x
        |]
    let expected = mempty
          { _register = [ ("x", Effects ["_prim_int_print"]) ]
          , _function = [ ("grinMain", Effects ["_prim_int_print"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "fun_print" $ do
    let exp = [prog|
          grinMain = f 0

          f x = _prim_int_print x
        |]
    let expected = mempty
          { _register = []
          , _function = [ ("f", Effects ["_prim_int_print"])
                        , ("grinMain", Effects ["_prim_int_print"])
                        ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bound_fun_print" $ do
    let exp = [prog|
          grinMain =
            y <- f 0
            pure y

          f x = _prim_int_print x
        |]
    let expected = mempty
          { _register = [ ("y", Effects ["_prim_int_print"]) ]
          , _function = [ ("f", Effects ["_prim_int_print"])
                        , ("grinMain", Effects ["_prim_int_print"])
                        ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bound_in_fun_print" $ do
    let exp = [prog|
          grinMain =
            z <- f 0
            pure z

          f x =
            y <- _prim_int_print x
            pure y
        |]
    let expected = mempty
          { _register = [ ("y", Effects ["_prim_int_print"])
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
              (COne) -> _prim_int_print 0
              (CTwo) -> _prim_string_print #"asd"
              (CFoo) -> pure ()
            pure y
        |]
    let expected = mempty
          { _register = [ ("y", Effects ["_prim_int_print", "_prim_string_print"])
                        , ("n", Effects [])
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
              (COne) -> f 0
              (CTwo) -> g #"asd"
              (CFoo) -> h
            pure y

          f x1 = _prim_int_print x1
          g x2 = _prim_string_print x2
          h    = pure ()
        |]
    let expected = mempty
          { _register = [ ("y", Effects ["_prim_int_print", "_prim_string_print"])
                        , ("n", Effects [])
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
              (COne) ->
                z <- case n of
                  (COne) -> _prim_int_print 0
                  (CTwo) -> _prim_error "Never should have come here"
                pure z
              (CTwo) -> _prim_string_print #"asd"
              (CFoo) -> pure ()
            pure y
        |]
    let expected = mempty
          { _register = [ ("y", Effects ["_prim_int_print", "_prim_string_print", "_prim_error"])
                        , ("z", Effects ["_prim_int_print", "_prim_error"])
                        , ("n", Effects [])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_int_print", "_prim_string_print", "_prim_error"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    calculated `sameAs` expected

  it "bind_sequence" $ do
    let exp = [prog|
          grinMain =
            x1 <- pure (COne)
            x2 <- _prim_int_print 0
            x3 <- pure 0
            x4 <- _prim_int_add 0 0
            x5 <- _prim_string_print #"asd"
            pure ()
        |]
    let expected = mempty
          { _register = [ ("x1", Effects [])
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
            y2 <- pure (CInt 0)
            y3 <- g k
            pure ()

          f n =
            x1 <- pure (COne)
            x2 <- _prim_int_print n
            x3 <- pure n
            x4 <- _prim_int_add n n
            x5 <- _prim_string_print #"asd"
            pure ()

          g m =
            z1 <- pure (CTwo)
            z2 <- pure n
            z3 <- _prim_error "ERROR"
            z4 <- _prim_int_add n n
            pure ()
        |]
    let expected = mempty
          { _register = [ ("k", Effects [])
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
            x0 <- _prim_string_print "Hello World!"
            x1 <- _prim_read_string
            x2 <- "newPrimSomething#" $
            x3 <- _prim_string_concat "Hello" "World"
            x4 <- newPrimSomething
            pure ()
        |]
    let expected = mempty
          { _register = [ ("x0", Effects ["_prim_string_print"])
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
            x0 <- _prim_string_print "Hello World!"
            x1 <- _prim_read_string
            x2 <- "newPrimSomething#" $
            x3 <- _prim_string_concat "Hello" "World"
            x4 <- newPrimSomething
            pure ()
        |]
    let expected = mempty
          { _register = [ ("x0", Effects ["_prim_string_print"])
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
            x0 <- _prim_string_print "Hello World!"
            pure x
        |]
    let expected = mempty
          { _register = [ ("x0", Effects ["_prim_string_print"])
                        , ("x1", Effects ["_prim_read_string"])
                        , ("x2", Effects ["newPrimSomething#"])
                        , ("x3", Effects [])
                        , ("x4", Effects [])
                        ]
          , _function = [ ("grinMain", Effects ["_prim_string_print", "_prim_read_string", "newPrimSomething#"]) ]
          }
        calculated = (calcEffects exp) { _external = mempty }
    (return $! calculated) `shouldThrow` anyException
