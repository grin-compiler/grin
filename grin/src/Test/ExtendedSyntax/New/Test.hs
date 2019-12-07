{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Test.ExtendedSyntax.New.Test where

import Data.Text (Text, pack)
import Data.Bifunctor (second)

import Control.Monad (forM_)

import Test.Hspec (Spec, describe)

import Grin.ExtendedSyntax.TH (expr)
import Grin.ExtendedSyntax.Grin (Exp)
import Grin.ExtendedSyntax.TypeEnv (TypeEnv)
import Grin.ExtendedSyntax.Pretty (PP(..))

type SpecWithProg = Exp -> Spec

type TestExpContext = (String, (TypeEnv, Exp) -> (TypeEnv, Exp))

testExprContext :: (((TypeEnv, Exp) -> (TypeEnv, Exp)) -> Spec) -> Spec
testExprContext mkSpec = forM_ contexts $ \(label, ctx) -> describe (concat ["(", label, ")"]) $ mkSpec ctx

contexts :: [TestExpContext]
contexts =
  [ emptyCtx
  , lastBindR
  , bindL 0
  , lastBindL 0
  , firstAlt
  , middleAlt
  , lastAlt
  ]

emptyCtx :: TestExpContext
emptyCtx = ("empty", id)

bindL :: Int -> TestExpContext
bindL (pack . show -> n) = ("bind left", second tr) where
  tr (exprText -> e) = [expr|
      fb$n <- do
        $e
      _prim_int_print 1
    |]

lastBindL :: Int -> TestExpContext
lastBindL (pack . show -> n) = ("last bind left", second tr) where
  tr (exprText -> e) = [expr|
      md$n <- do
        _prim_int_print 42
        $e
      _prim_int_print 1
    |]

firstAlt :: TestExpContext
firstAlt = ("first alt", second tr) where
  tr (exprText -> e) = [expr|
      case 1 of
        1 -> _prim_int_print 42
             $e
        2 -> _prim_int_print 1
        3 -> _prim_int_print 1
    |]

middleAlt :: TestExpContext
middleAlt = ("middle alt", second tr) where
  tr (exprText -> e) = [expr|
      case 1 of
        1 -> _prim_int_print 1
        2 -> _prim_int_print 1
             $e
        3 -> _prim_int_print 1
    |]

lastAlt :: TestExpContext
lastAlt = ("last alt", second tr) where
  tr (exprText -> e) = [expr|
      case 1 of
        1 -> _prim_int_print 1
        2 -> _prim_int_print 1
        3 -> _prim_int_print 1
             $e
    |]

lastBindR :: TestExpContext
lastBindR = ("last bind right", second tr) where
  tr (exprText -> e) = [expr|
      _prim_int_print 42
      $e
    |]

exprText :: Exp -> Text
exprText = pack . show . PP
