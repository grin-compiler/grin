module Parse.Tests.Undefined where

import System.FilePath

import Grin.Grin
import Grin.Parse
import Grin.Pretty
import Grin.TypeEnvDefs

import Test.Test
import Test.Hspec
import Test.Assertions

import Parse.Tests.Util

undefinedSrc :: FilePath
undefinedSrc = parseExamples </> "undefined.grin"

-- parse (1), pretty print, parse (2)
-- if the AST from (1) and (2) are the same, then the test passes
undefinedAstParseSpec :: Exp -> Spec
undefinedAstParseSpec ast = it "undefined_ast_parse" $ ast `sameAs` ast'
  where ast' = parseProg . show . WPP $ ast
