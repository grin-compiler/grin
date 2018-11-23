module Parse.Tests.StoreUndefined where

import System.FilePath

import Data.Text (pack)

import Grin.Grin
import Grin.Parse
import Grin.Pretty
import Grin.TypeEnvDefs

import Test.Test
import Test.Hspec
import Test.Assertions

import Parse.Tests.Util

storeUndefinedSrc :: FilePath
storeUndefinedSrc = parseExamples </> "store_undefined.grin"

-- parse (1), pretty print, parse (2)
-- if the AST from (1) and (2) are the same, then the test passes
storeUndefinedAstParseSpec :: Exp -> Spec
storeUndefinedAstParseSpec ast = it "store_undefined_ast_parse" $ ast `sameAs` ast'
  where ast' = parseProg . pack . show . WPP $ ast
