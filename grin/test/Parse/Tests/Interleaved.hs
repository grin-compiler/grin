module Parse.Tests.Interleaved where

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

interleavedSrc :: FilePath
interleavedSrc = parseExamples </> "interleaved.grin"

-- parse (1), pretty print, parse (2)
-- if the AST from (1) and (2) are the same, then the test passes
interleavedAstParseSpec :: Exp -> Spec
interleavedAstParseSpec ast = it "interleaved_ast_parse" $ ast `sameAs` ast'
  where ast' = parseProg . pack . show . WPP $ ast

-- this is just comparing the two parsing methods
-- parsing type annotations interleaved with code
-- and parsing pretty printed type environment
-- if both yield the same result, then the test passes 
interleavedTypeEnvParseSpec :: TypeEnv -> Spec
interleavedTypeEnvParseSpec env = it "interleaved_type_env_parse" $ env `sameAs` env'
  where env' = parseTypeEnv . pack . show . WPP $ env