{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.Megaparsec as M

import Options.Applicative

import Grin
import ParseGrin hiding (value)
import Pipeline

data Options = Options
  { optFiles     :: [FilePath]
  , optTrans     :: [Pipeline]
  , optOutputDir :: FilePath
  } deriving Show

flg c l h = flag' c (mconcat [long l, help h])
flg' c s l h = flag' c (mconcat [short s, long l, help h])

transformOpts :: Parser Transformation
transformOpts =
      flg CaseSimplification "cs" "Case Simplification"
  <|> flg SplitFetch "sf" "Split Fetch"
  <|> flg Vectorisation "v" "Vectorisation"
  <|> flg RegisterIntroduction "ri" "Register Introduction"
  <|> flg InlineEval "ie" "Inline Eval"
  <|> flg InlineApply "ia" "Inline Apply"
  <|> flg BindNormalisation "bn" "Bind Normalisation"
  <|> flg RightHoistFetch "rhf" "Right Hoist Fetch"
  <|> flg GenerateEval "ge" "Generate Eval"
  <|> flg ConstantFolding "cfl" "Constant Folding"
  <|> flg EvaluatedCaseElimination "ece" "Evaluated Case Elimination"
  <|> flg TrivialCaseElimination "tce" "Trivial Case Elimination"
  <|> flg SparseCaseOptimisation "sco" "Sparse Case Optimisation"
  <|> flg UpdateElimination "ue" "Update Elimination"
  <|> flg CopyPropagation "cp" "Copy Propagation"
  <|> flg ConstantPropagation "cnp" "Constant Propagation"
  <|> flg DeadProcedureElimination "dpe" "Dead Procedure Elimination"
  <|> flg DeadParameterElimination "dae" "Dead Parameter Elimination"
  <|> flg DeadVariableElimination "dve" "Dead Variable Elimination"
  <|> flg CommonSubExpressionElimination "cse" "Common Sub-Expression Elimination"
  <|> flg CaseCopyPropagation "ccp" "Case Copy Propagation"
  <|> flg GeneralizedUnboxing "gub" "GeneralizedUnboxing"
  <|> flg ArityRaising "ar" "Arity Raising"
  <|> flg CaseHoisting "ch" "Case Hoisting"
  <|> flg LateInlining "li" "Late Inlining"

pipelineOpts :: Parser Pipeline
pipelineOpts =
      flg (HPT CompileHPT) "compile-hpt" "Compiles heap-points-to analysis machine"
  <|> flg (HPT PrintHPTCode) "print-hpt-code" "Prints the heap-points-to analysis machine"
  <|> flg (HPT RunHPTPure) "run-hpt-pure" "Runs the heap-points-to analysis machine via pure interpreter"
  <|> flg (HPT PrintHPTResult) "print-hpt-result" "Prints the heap-points-to analysis result"
  <|> flg' Lint 'l' "lint" "Checks the well-formedness of the actual grin code"
  <|> flg' (PrintGrin id) 'p' "print-grin" "Prints the actual grin code"
  <|> flg PrintTypeEnv "te" "Prints type env"
  <|> flg' (Pass [HPT CompileHPT, HPT RunHPTPure]) 't' "hpt" "Compiles and runs the heap-points-to analysis"
  <|> flg PureEval "eval" "Evaluate the grin program (pure)"
  <|> flg JITLLVM "llvm" "JIT with LLVM"
  <|> flg PrintAST "ast" "Print the Abstract Syntax Tree"
  <|> (SaveLLVM <$> (strOption (mconcat
        [ long "save-llvm"
        , help "Save the generated llvm"
        ])))
  <|> (SaveGrin <$> (strOption (mconcat
        [ long "save-grin"
        , help "Save the generated grin"
        ])))
  <|> (T <$> transformOpts)

options :: IO Options
options = execParser $ info
  (pipelineArgs <**> helper)
  (mconcat
    [ fullDesc
    , progDesc "grin compiler"
    , header "grin compiler"
    ])
  where
    pipelineArgs = Options
      <$> some (argument str (metavar "FILES..."))
      <*> many pipelineOpts
      <*> strOption (mconcat
            [ short 'o'
            , long "output-dir"
            , help "Output directory for generated files"
            , value "./output"
            ])

main :: IO ()
main = do
  Options files steps outputDir <- options
  forM_ files $ \fname -> do
    content <- readFile fname
    let program = either (error . M.parseErrorPretty' content) id $ parseGrin fname content
        opts = PipelineOpts { _poOutputDir = outputDir }
    case steps of
      [] -> void $ optimize opts program prePipeline postPipeline
      _  -> void $ pipeline opts program steps

prePipeline :: [Pipeline]
prePipeline =
  [ HPT CompileHPT
  , HPT PrintHPTCode
  , PrintGrin ondullblack
  , HPT RunHPTPure
  , HPT PrintHPTResult
  , SaveLLVM "high-level-code"
  ]

postPipeline :: [Pipeline]
postPipeline =
  [ SaveLLVM "high-level-opt-code"
  , JITLLVM -- TODO: Remove this.
  ]
