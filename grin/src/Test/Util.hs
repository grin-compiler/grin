{-# LANGUAGE OverloadedStrings #-}
module Test.Util where

-- TODO: Remove this module

import System.FilePath

import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)

import Grin.Grin
import Grin.Parse

import Test.Hspec
import Test.Assertions

cInt :: Tag
cInt = Tag C "Int"

cBool :: Tag
cBool = Tag C "Bool"

cWord :: Tag
cWord = Tag C "Word"

cBoolH :: Tag
cBoolH = Tag C "BoolH"

cWordH :: Tag
cWordH = Tag C "WordH"

cOne :: Tag
cOne = Tag C "One"

cTwo :: Tag
cTwo = Tag C "Two"

cNode :: Tag
cNode = Tag C "Node"

cFoo :: Tag
cFoo = Tag C "Foo"

cBar :: Tag
cBar = Tag C "Bar"

cNil :: Tag
cNil = Tag C "Nil"

cCons :: Tag
cCons = Tag C "Cons"

-- name ~ name of the test case, and also the grin source file
mkBeforeAfterTestCase :: String ->
                         FilePath ->
                         FilePath ->
                         (FilePath, FilePath, FilePath -> Exp -> Spec)
mkBeforeAfterTestCase name beforeDir afterDir = (before, after, specFun)
  where before = beforeDir </> name <.> "grin"
        after  = afterDir  </> name <.> "grin"
        specFun after' transformed = do
          expected <- runIO $ T.readFile after'
          let expected' = parseProg expected
          it name $ transformed `sameAs` expected'
