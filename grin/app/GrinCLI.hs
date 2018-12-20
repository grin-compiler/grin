{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Data.Text.IO as Text
import qualified Text.Megaparsec as M

import Options.Applicative

import Grin.Grin
import Grin.Parse hiding (value)
import Pipeline.Pipeline

data Options = Options
  { optFiles     :: [FilePath]
  , optTrans     :: [PipelineStep]
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
  <|> flg ProducerNameIntroduction "pni" "Producer Name Introduction"
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
  <|> flg DeadCodeElimination "dce" "Dead Code Elimination"
  <|> flg DeadDataElimination "dde" "Dead Data Elimination"
  <|> flg DeadFunctionElimination "dfe" "Dead Function Elimination"
  <|> flg DeadParameterElimination "dpe" "Dead Parameter Elimination"
  <|> flg DeadVariableElimination "dve" "Dead Variable Elimination"
  <|> flg SimpleDeadFunctionElimination "sdfe" "Dead Procedure Elimination"
  <|> flg SimpleDeadVariableElimination "sdve" "Simple Dead Variable Elimination"
  <|> flg SimpleDeadParameterElimination "sdpe" "Simple Dead Parameter Elimination"
  <|> flg CommonSubExpressionElimination "cse" "Common Sub-Expression Elimination"
  <|> flg CaseCopyPropagation "ccp" "Case Copy Propagation"
  <|> flg GeneralizedUnboxing "gub" "GeneralizedUnboxing"
  <|> flg ArityRaising "ar" "Arity Raising"
  <|> flg CaseHoisting "ch" "Case Hoisting"
  <|> flg LateInlining "li" "Late Inlining"
  <|> flg MangleNames "mn" "Mangle Names"
  <|> flg StaticSingleAssignment "ssa" "Fix up Static Single Assignment property"
  <|> flg NonSharedElimination "nse" "Non Shared Elimination"

pipelineOpts :: Parser PipelineStep
pipelineOpts =
      flg Optimize "optimize" "Iteratively performs optimizations on the GRIN code until it can no longer be optimized"
  <|> flg (HPT Compile) "compile-hpt" "Compiles heap-points-to analysis machine"
  <|> flg (HPT Optimise) "optimize-hpt" "Optimizes heap-points-to analysis abstract program"
  <|> flg (HPT PrintProgram) "print-hpt-code" "Prints the heap-points-to analysis machine"
  <|> flg (HPT RunPure) "run-hpt-pure" "Runs the heap-points-to analysis machine via pure interpreter"
  <|> flg (HPT PrintResult) "print-hpt-result" "Prints the heap-points-to analysis result"
  <|> flg (CBy Compile) "compile-cby" "Compiles created-by analysis machine"
  <|> flg (CBy Optimise) "optimize-cby" "Optimizes created-by analyis abstract program"
  <|> flg (CBy PrintProgram) "print-cby-code" "Prints the created-by analysis machine"
  <|> flg (CBy RunPure) "run-cby-pure" "Runs the created-by analysis machine via pure interpreter"
  <|> flg (CBy PrintResult) "print-cby-result" "Prints the created-by analysis result"
  <|> flg (LVA Compile) "compile-lva" "Compiles live variable analysis machine"
  <|> flg (LVA Optimise) "optimize-lva" "Optimizes live variable analysis abstract program"
  <|> flg (LVA PrintProgram) "print-lva-code" "Prints the live variable analysis machine"
  <|> flg (LVA RunPure) "run-lva-pure" "Runs the live variable analysis machine via pure interpreter"
  <|> flg (LVA PrintResult) "print-lva-result" "Prints the live variable analysis result"
  <|> flg (Sharing Compile) "compile-sharing" "Compiles sharing analysis machine"
  <|> flg (Sharing Optimise) "optimize-sharing" "Optimizes sharing analyis abstract program"
  <|> flg (Sharing PrintProgram) "print-sharing-code" "Prints the sharing analysis machine"
  <|> flg (Sharing RunPure) "run-sharing-pure" "Runs the sharing analysis machine via pure interpreter"
  <|> flg (Sharing PrintResult) "print-sharing-result" "Prints the sharing analysis result"
  <|> flg' (Eff CalcEffectMap) 'e' "em" "Calculate the effect for functions"
  <|> flg (Eff PrintEffectMap) "pe" "Print effect map"
  <|> flg' Lint 'l' "lint" "Checks the well-formedness of the actual grin code"
  <|> flg' (PrintGrin id) 'p' "print-grin" "Prints the actual grin code"
  <|> flg PrintTypeAnnots "print-type-annots" "Prints the type env calculated from the annotations in the source"
  <|> flg PrintTypeEnv "te" "Prints type env"
  <|> flg' (Pass [HPT Compile, HPT RunPure]) 't' "hpt" "Compiles and runs the heap-points-to analysis"
  <|> flg' (Pass [CBy Compile, CBy RunPure]) 'c' "cby" "Compiles and runs the created-by analysis"
  <|> flg' (Pass [LVA Compile, LVA RunPure]) 'v' "lva" "Compiles and runs the live variable analysis"
  <|> flg' (Pass [Sharing Compile, Sharing RunPure]) 's' "sharing" "Compiles and runs the sharing analysis"
  <|> flg (Pass [HPT Compile, HPT Optimise, HPT RunPure]) "hpt-opt" "Compiles, optimizes and runs the heap-points-to analysis"
  <|> flg (Pass [CBy Compile, CBy Optimise, CBy RunPure]) "cby-opt" "Compiles, optimizes and runs the created-by analysis"
  <|> flg (Pass [LVA Compile, LVA Optimise, LVA RunPure]) "lva-opt" "Compiles, optimizes and runs the live variable analysis"
  <|> flg (Pass [Sharing Compile, Sharing Optimise, Sharing RunPure]) "sharing-opt" "Compiles, optimizes and runs the sharing analysis"
  <|> flg  (Pass [LVA Compile, CBy Compile, RunCByWithLVA]) "cby-with-lva" "Compiles the live variable and created-by analyses, then runs the created-by analysis using the LVA result"
  <|> flg PureEval "eval" "Evaluate the grin program (pure)"
  <|> flg JITLLVM "llvm" "JIT with LLVM"
  <|> flg PrintAST "ast" "Print the Abstract Syntax Tree"
  <|> (SaveLLVM True <$> (strOption (mconcat
        [ long "save-llvm"
        , help "Save the generated llvm"
        ])))
  <|> (SaveGrin . Abs <$> (strOption (mconcat
        [ long "save-grin"
        , help "Save the generated grin"
        ])))
  <|> (T <$> transformOpts)
  <|> flg ConfluenceTest "confluence-test" "Checks transformation confluence by generating random two pipelines which reaches the fix points."
  <|> flg PrintErrors "print-errors" "Prints the error log"

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
            , value "./.output"
            ])

main :: IO ()
main = do
  Options files steps outputDir <- options
  forM_ files $ \fname -> do
    content <- Text.readFile fname
    let (typeEnv, program)  = either (error . M.parseErrorPretty' content) id $ parseGrinWithTypes fname content
        opts                = defaultOpts { _poOutputDir = outputDir, _poFailOnLint = True }
    case steps of
      [] -> void $ optimize opts program prePipeline postPipeline
      _  -> void $ pipeline opts typeEnv program steps

prePipeline :: [PipelineStep]
prePipeline = defaultOnChange

postPipeline :: [PipelineStep]
postPipeline =
  [ SaveLLVM True "high-level-opt-code"
  , JITLLVM -- TODO: Remove this.
  , PrintTypeEnv
  , PrintGrin ondullblack
  ]
