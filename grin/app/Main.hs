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

transformOpts :: Parser Transformation
transformOpts =
      flg CaseSimplification "cs" "Case Simplification"
  <|> flg SplitFetch "sf" "Split Fetch"
  <|> flg Vectorisation "v" "Vectorisation"
  <|> flg RegisterIntroduction "ri" "Register Introduction"
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
  <|> flg DeadVariableElimination "dve" "Dead Variable Elimination"
  <|> flg CommonSubExpressionElimination "cse" "Common Sub-Expression Elimination"
  <|> flg CaseCopyPropagation "ccp" "Case Copy Propagation"
  <|> flg GeneralizedUnboxing "gub" "GeneralizedUnboxing"
  <|> flg ArityRaising "ar" "Arity Raising"

pipelineOpts :: Parser Pipeline
pipelineOpts =
      flg (HPT CompileHPT) "compile-hpt" "Compiles heap-points-to analysis machine"
  <|> flg (HPT PrintHPT) "print-hpt" "Prints the heap-points-to analysis machine"
  <|> flg (HPT RunHPTPure) "run-hpt-pure" "Runs the heap-points-to analysis machine via pure interpreter"
  <|> flg (HPT PrintHPTResult) "print-hpt-result" "Prints the heap-points-to analysis result"
  <|> flg (PrintGrin id) "print-grin" "Prints the actual grin code"
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

defaultPipeline :: Options -> Options
defaultPipeline = \case
  Options files [] output ->
    Options
      files
      [ HPT CompileHPT
      , HPT PrintHPT
      , PrintGrin ondullblack
      , HPT RunHPTPure
      , HPT PrintHPTResult
      , SaveLLVM "high-level-code"
      , JITLLVM

      , T UpdateElimination
      , SaveGrin "UpdateElimination"
      , T BindNormalisation
      , SaveGrin "UpdateElimination"
      , PrintGrin ondullblack

      , T SparseCaseOptimisation
      , SaveGrin "SparseCaseOptimisation"
      , T BindNormalisation
      , SaveGrin "SparseCaseOptimisation"
      , PrintGrin ondullcyan

      , T EvaluatedCaseElimination
      , SaveGrin "EvaluatedCaseElimination"
      , T BindNormalisation
      , SaveGrin "EvaluatedCaseElimination"
      , PrintGrin ondullblack

      , T TrivialCaseElimination
      , SaveGrin "TrivialCaseElimination"
      , T BindNormalisation
      , SaveGrin "TrivialCaseElimination"
      , PrintGrin ondullcyan

      , T CopyPropagation
      , SaveGrin "CopyPropagation"
      , T BindNormalisation
      , SaveGrin "CopyPropagation"
      , PrintGrin ondullblack

      , T ConstantPropagation
      , SaveGrin "ConstantPropagation"
      , T BindNormalisation
      , SaveGrin "ConstantPropagation"
      , PrintGrin ondullblack

      , T DeadProcedureElimination
      , SaveGrin "DeadProcedureElimination"
      , T BindNormalisation
      , SaveGrin "DeadProcedureElimination"
      , PrintGrin ondullblack

      , T DeadVariableElimination
      , SaveGrin "DeadVariableElimination"
      , T BindNormalisation
      , SaveGrin "DeadVariableElimination"
      , PrintGrin ondullblack

      , T CommonSubExpressionElimination
      , SaveGrin "CommonSubExpressionElimination"
      , T BindNormalisation
      , SaveGrin "CommonSubExpressionElimination"
      , PrintGrin ondullblack

      , T CaseCopyPropagation
      , SaveGrin "CaseCopyPropagation"
      , T BindNormalisation
      , SaveGrin "CaseCopyPropagation"
      , PrintGrin ondullblack

      , T GeneralizedUnboxing
      , SaveGrin "GeneralizedUnboxing"
      , T BindNormalisation
      , SaveGrin "GeneralizedUnboxing"
      , PrintGrin ondullblack

      , T ArityRaising
      , SaveGrin "ArityRaising"
      , T BindNormalisation
      , SaveGrin "ArityRaising"
      , PrintGrin ondullblack

{- TODO: Enable simplificaiton transformations in the pipeline.
      , T Vectorisation
      , SaveGrin "Vectorisation"
      , T BindNormalisation
      , SaveGrin "Vectorisation"
      , PrintGrin ondullblack
      , T CaseSimplification
      , SaveGrin "CaseSimplification"
      , T BindNormalisation
      , SaveGrin "CaseSimplification"
      , PrintGrin ondullcyan
      , T SplitFetch
      , SaveGrin "SplitFetch"
      , T BindNormalisation
      , SaveGrin "SplitFetch"
      , PrintGrin ondullblack
      , T RightHoistFetch
      , SaveGrin "RightHoistFetch"
      , T BindNormalisation
      , SaveGrin "RightHoistFetch"
      , PrintGrin ondullcyan
      , T RegisterIntroduction
      , SaveGrin "RegisterIntroduction"
      , T BindNormalisation
      , SaveGrin "RegisterIntroduction"
      , PrintGrin ondullblack
      , SaveLLVM "low-level-code"
-}
      , SaveLLVM "high-level-opt-code"
      , JITLLVM
      ]
      output
  opts -> opts

main :: IO ()
main = do
  Options files steps outputDir <- defaultPipeline <$> options
  forM_ files $ \fname -> do
    content <- readFile fname
    let program = either (error . M.parseErrorPretty' content) id $ parseGrin fname content
        opts = PipelineOpts { _poOutputDir = outputDir }
    pipeline opts program steps
