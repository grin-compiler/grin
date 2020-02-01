{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Test.ExtendedSyntax.Util where

-- TODO: Remove this module

import System.FilePath

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Text (Text)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as L (isSuffixOf)
import qualified Data.Vector as V
import qualified Data.Text.IO as T (readFile)

import System.Directory (getCurrentDirectory)

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Parse
import Grin.ExtendedSyntax.PrimOpsPrelude
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result as HPT

import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)
import Test.ExtendedSyntax.Assertions

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

cNope :: Tag
cNope = Tag C "Nope"

cNopeH :: Tag
cNopeH = Tag C "NopeH"

loc :: HPT.Loc -> TypeSet
loc = tySetFromTypes . pure . HPT.T_Location

unspecLoc :: TypeSet
unspecLoc = tySetFromTypes [HPT.T_UnspecifiedLocation]

mkNode :: [[HPT.SimpleType]] -> Vector (Set HPT.SimpleType)
mkNode = V.fromList . map Set.fromList

mkNodeSet :: [(Tag, [[HPT.SimpleType]])] -> NodeSet
mkNodeSet = HPT.NodeSet . Map.fromList . map (\(t,v) -> (t,mkNode v))

mkTySet :: [(Tag, [[HPT.SimpleType]])] -> TypeSet
mkTySet = tySetFromNodeSet . mkNodeSet

tySetFromNodeSet :: NodeSet -> TypeSet
tySetFromNodeSet = TypeSet mempty

tySetFromTypes :: [HPT.SimpleType] -> TypeSet
tySetFromTypes = flip TypeSet mempty . Set.fromList

mkSimpleMain :: HPT.SimpleType -> (TypeSet, Vector TypeSet)
mkSimpleMain t = (tySetFromTypes [t], mempty)

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

loadTestData :: FilePath -> IO Exp
loadTestData path = do
  pwd <- getCurrentDirectory
  -- There is a difference between the 'stack ghci --test' and 'stack test'.
  -- Stack test uses the grin/grin meanwhile stack ghci uses 'grin' directory
  let testDataDir = if | "/grin/grin" `L.isSuffixOf` pwd -> "test-data/ExtendedSyntax"
                       | "/grin"      `L.isSuffixOf` pwd -> "grin/test-data/ExtendedSyntax"
                       | otherwise -> error "Impossible: stack did not run inside the project dir."

  file <- T.readFile (testDataDir </> path)
  pure $ withPrimPrelude . parseProg $ file
