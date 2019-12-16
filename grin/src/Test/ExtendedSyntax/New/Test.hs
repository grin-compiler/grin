{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Test.ExtendedSyntax.New.Test where

import Data.Text (Text, pack)
import Data.Bifunctor (second)

import Control.Monad (forM_)

import Test.Hspec (Spec, describe)

import Grin.ExtendedSyntax.TH (expr)
import Grin.ExtendedSyntax.Grin (Exp)
import Grin.ExtendedSyntax.TypeEnv (TypeEnv, emptyTypeEnv)
import Grin.ExtendedSyntax.Pretty (PP(..))

type SpecWithProg = Exp -> Spec

type TestExpContext = (String, (TypeEnv, Exp) -> (TypeEnv, Exp))

testExprContext :: (((TypeEnv, Exp) -> (TypeEnv, Exp)) -> Spec) -> Spec
testExprContext mkSpec = forM_ contexts $ \(label, ctx) -> describe (concat ["(", label, ")"]) $ mkSpec ctx

testExprContextE :: ((Exp -> Exp) -> Spec) -> Spec
testExprContextE mkSpec =
  forM_ contexts $ \(label, ctx) ->
    describe (concat ["(", label, ")"]) $ mkSpec (\e -> snd $ ctx (emptyTypeEnv, e))

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
      pure ()
    |]

lastBindL :: Int -> TestExpContext
lastBindL (pack . show -> n) = ("last bind left", second tr) where
  tr (exprText -> e) = [expr|
      md$n <- do
        __1 <- pure ()
        $e
      pure ()
    |]

firstAlt :: TestExpContext
firstAlt = ("first alt", second tr) where
  tr (exprText -> e) = [expr|
      __1 <- pure 1
      case __1 of
        1 @ __2 ->
          __x <- pure ()
          $e
        2 @ __3 ->
          pure ()
        3 @ __4 ->
          pure ()
    |]

middleAlt :: TestExpContext
middleAlt = ("middle alt", second tr) where
  tr (exprText -> e) = [expr|
      __1 <- pure 1
      case __1 of
        1 @ __2 ->
          pure ()
        2 @ __3 ->
          __x <- pure ()
          $e
        3 @ __4 ->
          pure ()
    |]

lastAlt :: TestExpContext
lastAlt = ("last alt", second tr) where
  tr (exprText -> e) = [expr|
      __1 <- pure 1
      case __1 of
        1 @ __2 ->
          pure ()
        2 @ __3 ->
          pure ()
        3 @ __4 ->
          __x <- pure ()
          $e
    |]

lastBindR :: TestExpContext
lastBindR = ("last bind right", second tr) where
  tr (exprText -> e) = [expr|
      __1 <- pure ()
      $e
    |]

exprText :: Exp -> Text
exprText = pack . show . PP
