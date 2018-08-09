module Main where

import Control.Monad

import System.Environment
import System.Exit
import qualified Text.Megaparsec as M

import GhcDump.Util

import Frontend.Lambda.FromDumpCore
import Frontend.Lambda.Syntax
import Frontend.Lambda.Parse
import Frontend.Lambda.Pretty
import Frontend.Lambda.CodeGen
import Grin.Pretty
import Pipeline.Pipeline

import Text.PrettyPrint.ANSI.Leijen (ondullblack, putDoc, plain, pretty)

data Opts
  = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  }

showUsage = do putStrLn "Usage: ghc-core-grin <source-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.out") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

cg_main :: Opts -> IO ()
cg_main opts = do
  forM_ (inputs opts) $ \fname -> do
    coreModule <- readDump fname
    program <- codegenLambda coreModule
    putStrLn "\n* Lambda"
    putDoc (plain $ pretty program) >> putStrLn ""
    pure ()
    {-
    let lambdaGrin = codegenGrin program
    void $ pipeline pipelineOpts lambdaGrin
      [ SaveGrin "from-lambda.grin"
      , T GenerateEval
      , SaveGrin (output opts)
      , PrintGrin ondullblack
      ]
    -}

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else cg_main opts

pipelineOpts :: PipelineOpts
pipelineOpts = defaultOpts
  { _poOutputDir = "."
  , _poFailOnLint = True
  }
