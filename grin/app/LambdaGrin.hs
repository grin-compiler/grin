module Main where

import Control.Monad

import System.Environment
import System.Exit
import qualified Text.Megaparsec as M

import Frontend.Lambda.Syntax
import Frontend.Lambda.Parse
import Frontend.Lambda.Pretty
import Frontend.Lambda.CodeGen
import Pretty
import Pipeline

import Text.PrettyPrint.ANSI.Leijen (ondullblack)

data Opts
  = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  }

showUsage = do putStrLn "Usage: lambda-grin <source-files> [-o <output-file>]"
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
    content <- readFile fname
    let program = either (error . M.parseErrorPretty' content) id $ parseLambda fname content
    putStrLn "\n* Lambda"
    printLambda program
    let lambdaGrin = codegenGrin program
    void $ optimize pipelineOpts lambdaGrin
      -- pre
      [ SaveGrin "from-lambda"
      , PrintGrin ondullblack
      ]
      -- post
      [ SaveGrin "opt"
      , PrintGrin ondullblack
      , SaveLLVM "opt"
      , JITLLVM
      ]

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else cg_main opts

pipelineOpts :: PipelineOpts
pipelineOpts = PipelineOpts
  { _poOutputDir = "./lambda/"
  }
