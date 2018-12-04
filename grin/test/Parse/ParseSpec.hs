{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module Parse.ParseSpec where

import System.FilePath

import Test.IO
import Test.Hspec
import Test.Assertions

import Grin.Grin
import Grin.Parse
import Grin.TH
import Grin.Pretty
import Data.Text


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = describe "Parse" $ do

  it "interleaved_type_env_parse" $ do
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
    let env = parseMarkedTypeEnv exp
    env `sameAs` (parseTypeEnv . pack . show . WPP $ env)

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
    exp `sameAs` (parseProg . pack . show . WPP $ exp)

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
    exp `sameAs` (parseProg . pack . show . WPP $ exp)

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
    exp `sameAs` (parseProg . pack . show . WPP $ exp)
