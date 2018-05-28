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
    pipeline pipelineOpts lambdaGrin lambdaPipeLine

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else cg_main opts

pipelineOpts :: PipelineOpts
pipelineOpts = PipelineOpts
  { _poOutputDir = "./lambda/"
  }

simplifyingPipeline :: [Pipeline]
simplifyingPipeline =
  [ T CaseSimplification
--  , T SplitFetch -- has an issue
  , T RightHoistFetch
  ]

optimisingPipeline :: [Pipeline]
optimisingPipeline = concat $ replicate 10
  [ T CaseCopyPropagation
  , T CommonSubExpressionElimination
  , T ConstantPropagation
  , T CopyPropagation
  , T EvaluatedCaseElimination
  , T TrivialCaseElimination
  , T UpdateElimination
  , T ArityRaising -- has an issue ; always trigger -> unfolds the same expression multiple times
--  , T GeneralizedUnboxing -- has an issue ; mixes up simple type and node types
  , T SparseCaseOptimisation -- has an issue ; workaround ; llvm codegen validator should be smarter

--  , T Inlining -- integrate to the pipeline

  , T BindNormalisation
  , T DeadProcedureElimination
  , T DeadVariableElimination -- has an issue: removed effectful code, like print_int ; idris-grin codegen should treat unboxed units as grin unit value
  , T BindNormalisation
  ]

miscPipeline :: [Pipeline]
miscPipeline =
  [ T ConstantFolding
  ]


codegenPipeline :: [Pipeline]
codegenPipeline =
  [ PrintGrin ondullblack
  , SaveGrin "high-level-opt-code.grin"
  , SaveLLVM "high-level-opt-code"
  , PureEval
  , JITLLVM
  ]

lambdaPipeLine :: [Pipeline]
lambdaPipeLine =
  [ SaveGrin "from-lambda"
  , T DeadProcedureElimination
  , PrintGrin ondullblack
  , HPT CompileHPT
  , HPT PrintHPTCode
  , HPT RunHPTPure
  , HPT PrintHPTResult
  , SaveLLVM "high-level-code"
  , SaveGrin "high-level-code.grin"
  ]
  ++ optimisingPipeline
  ++ codegenPipeline

