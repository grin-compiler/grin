{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module Test.ExtendedSyntax.New.Test where

import Data.Text (Text, pack)
import Data.Bifunctor (second)

import Control.Monad (forM_, forM)

import Test.Hspec (Spec, describe)

import Grin.ExtendedSyntax.TH (expr)
import Grin.ExtendedSyntax.Grin (Exp, Name)
import Grin.ExtendedSyntax.TypeEnv (TypeEnv, emptyTypeEnv)
import Grin.ExtendedSyntax.Pretty (Pretty, PP(..))

import Transformations.ExtendedSyntax.Names (evalNameM, deriveNewName)

type SpecWithProg = Exp -> Spec

type TestExpContext = (String, (TypeEnv, Exp) -> (TypeEnv, Exp))

testExprContext :: (((TypeEnv, Exp) -> (TypeEnv, Exp)) -> Spec) -> Spec
testExprContext mkSpec = forM_ contexts $ \(label, ctx) -> describe (concat ["(", label, ")"]) $ mkSpec ctx

testExprContextIn :: [TestExpContext] -> (((TypeEnv, Exp) -> (TypeEnv, Exp)) -> Spec) -> Spec
testExprContextIn ctxs mkSpec = forM_ ctxs $ \(label, ctx) -> describe (concat ["(", label, ")"]) $ mkSpec ctx

testExprContextE :: ((Exp -> Exp) -> Spec) -> Spec
testExprContextE mkSpec =
  forM_ contexts $ \(label, ctx) ->
    describe (concat ["(", label, ")"]) $ mkSpec (\e -> snd $ ctx (emptyTypeEnv, e))

contexts :: [TestExpContext]
contexts =
  [ emptyCtx
  , lastBindR
  , bindL
  , lastBindL
  , firstAlt
  , middleAlt
  , lastAlt
  ]

emptyCtx :: TestExpContext
emptyCtx = ("empty", id)

-- NOTE: These contexts contain some names. Make sure not to use these in your test code!

deriveNames :: Exp -> [Name]
deriveNames e = fst <$> evalNameM e $ do
  forM [1..] $ \_ -> deriveNewName "ctxVar"

deriveNamesAsText :: Exp -> [Text]
deriveNamesAsText = map toText . deriveNames

toText :: Pretty a => a -> Text
toText = pack . show . PP

bindL :: TestExpContext
bindL = ("bind left", second tr) where
  tr e
    | (v:_) <- deriveNamesAsText e
    , eText <- toText e = [expr|
      $v <- do
        $eText
      pure ()
    |]

lastBindL :: TestExpContext
lastBindL = ("last bind left", second tr) where
  tr e
    | (v1:v2:_) <- deriveNamesAsText e
    , eText <- toText e = [expr|
      $v1 <- do
        $v2 <- pure ()
        $eText
      pure ()
    |]

firstAlt :: TestExpContext
firstAlt = ("first alt", second tr) where
  tr e
    | (scrut:v1:v2:v3:x:_) <- deriveNamesAsText e
    , eText <- toText e = [expr|
      $scrut <- pure 1
      case $scrut of
        1 @ $v1 ->
          $x <- pure ()
          $eText
        2 @ $v2 ->
          pure ()
        3 @ $v3 ->
          pure ()
    |]

middleAlt :: TestExpContext
middleAlt = ("middle alt", second tr) where
  tr e
    | (scrut:v1:v2:v3:x:_) <- deriveNamesAsText e
    , eText <- toText e = [expr|
      $scrut <- pure 1
      case $scrut of
        1 @ $v1 ->
          pure ()
        2 @ $v2 ->
          $x <- pure ()
          $eText
        3 @ $v3 ->
          pure ()
    |]

lastAlt :: TestExpContext
lastAlt = ("last alt", second tr) where
  tr e
    | (scrut:v1:v2:v3:x:_) <- deriveNamesAsText e
    , eText <- toText e = [expr|
      $scrut <- pure 1
      case $scrut of
        1 @ $v1 ->
          pure ()
        2 @ $v2 ->
          pure ()
        3 @ $v3 ->
          $x <- pure ()
          $eText
    |]

lastBindR :: TestExpContext
lastBindR = ("last bind right", second tr) where
  tr e
    | (v:_) <- deriveNamesAsText e
    , eText <- toText e = [expr|
      $v <- pure ()
      $eText
    |]
