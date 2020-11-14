{-# LANGUAGE LambdaCase #-}
module CLI.Lib where

import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Char
import Data.Void
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Data.Text.IO as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Data.Binary as Binary

import Options.Applicative
import System.IO

import Grin.Grin
import Grin.PrimOpsPrelude
import Grin.Parse
import Grin.Nametable as Nametable
import Pipeline.Pipeline



data Options = Options
  { optFiles     :: [FilePath]
  , optTrans     :: [PipelineStep]
  , optOutputDir :: FilePath
  , optNoPrelude :: Bool
  , optQuiet     :: Bool
  , optLoadBinary :: Bool
  , optSaveBinary :: Bool
  , optCFiles     :: [FilePath]
  , optDontFailOnLint :: Bool
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
  <|> flg BindingPatternSimplification "bps" "Binding Pattern Simplification"
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
  <|> (HPT . SaveProgram <$> (strOption (mconcat [long "save-hpt-code", help "Saves the heap-points-to analysis machine"])))
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
  <|> flg (ET Compile) "compile-et" "Compiles effect tracking analysis machine"
  <|> flg (ET Optimise) "optimize-et" "Optimizes effect tracking analysis abstract program"
  <|> flg (ET PrintProgram) "print-et-code" "Prints the effect tracking analysis machine"
  <|> flg (ET RunPure) "run-et-pure" "Runs the effect tracking analysis machine via pure interpreter"
  <|> flg (ET PrintResult) "print-et-result" "Prints the effect tracking analysis result"
  <|> flg (Sharing Compile) "compile-sharing" "Compiles sharing analysis machine"
  <|> flg (Sharing Optimise) "optimize-sharing" "Optimizes sharing analyis abstract program"
  <|> flg (Sharing PrintProgram) "print-sharing-code" "Prints the sharing analysis machine"
  <|> flg (Sharing RunPure) "run-sharing-pure" "Runs the sharing analysis machine via pure interpreter"
  <|> flg (Sharing PrintResult) "print-sharing-result" "Prints the sharing analysis result"
  <|> flg' (Eff CalcEffectMap) 'e' "em" "Calculate the effect for functions"
  <|> flg (Eff PrintEffectMap) "pe" "Print effect map"
  <|> flg' Lint 'l' "lint" "Checks the well-formedness of the actual grin code"
  <|> flg' (SimplePrintGrin id) 'p' "simple-print-grin" "Print the actual grin code without externals" <|> printGrinWithOpt
  <|> flg PrintTypeAnnots "print-type-annots" "Prints the type env calculated from the annotations in the source"
  <|> flg PrintTypeEnv "te" "Prints type env"
  <|> flg' (Pass [HPT Compile, HPT RunPure]) 't' "hpt" "Compiles and runs the heap-points-to analysis"
  <|> flg' (Pass [CBy Compile, CBy RunPure]) 'c' "cby" "Compiles and runs the created-by analysis"
  <|> flg' (Pass [LVA Compile, LVA RunPure]) 'v' "lva" "Compiles and runs the live variable analysis"
  <|> flg  (Pass [ET Compile,  ET RunPure])      "et"  "Compiles and runs the effect tracking analysis"
  <|> flg' (Pass [Sharing Compile, Sharing RunPure]) 's' "sharing" "Compiles and runs the sharing analysis"
  <|> flg (Pass [HPT Compile, HPT Optimise, HPT RunPure]) "hpt-opt" "Compiles, optimizes and runs the heap-points-to analysis"
  <|> flg (Pass [CBy Compile, CBy Optimise, CBy RunPure]) "cby-opt" "Compiles, optimizes and runs the created-by analysis"
  <|> flg (Pass [LVA Compile, LVA Optimise, LVA RunPure]) "lva-opt" "Compiles, optimizes and runs the live variable analysis"
  <|> flg (Pass [ET Compile, ET Optimise, ET RunPure]) "et-opt" "Compiles, optimizes and runs the effect tracking analysis"
  <|> flg (Pass [Sharing Compile, Sharing Optimise, Sharing RunPure]) "sharing-opt" "Compiles, optimizes and runs the sharing analysis"
  <|> flg  (Pass [LVA Compile, CBy Compile, RunCByWithLVA]) "cby-with-lva" "Compiles the live variable and created-by analyses, then runs the created-by analysis using the LVA result"
  <|> flg DeadCodeElimination "dce" "Dead Code Elimination"
  <|> flg (PureEval False) "eval" "Evaluate the grin program (pure)"
  <|> (PureEval <$> (option auto (mconcat [long "eval-with-statistics", help "Evaluate the grin program (pure) and render heap statistics."])))
  <|> flg JITLLVM "llvm" "JIT with LLVM"
  <|> flg PrintAST "ast" "Print the Abstract Syntax Tree"
  <|> (SaveExecutable False . Abs <$> (strOption (mconcat [short 'o', long "save-elf", help "Save an executable ELF"])))
  <|> (SaveExecutable True . Abs <$> (strOption (mconcat [short 'o', long "save-elf-dbg", help "Save an executable ELF with debug symbols"])))
  <|> (SaveLLVM . Abs <$> (strOption (mconcat [long "save-llvm", help "Save the generated llvm"])))
  <|> (SaveGrin . Abs <$> (strOption (mconcat [long "save-grin", help "Save the generated grin"])))
  <|> (SaveBinary     <$> (strOption (mconcat [long "save-binary", help "Save the generated grin in binary format"])))
  <|> (T <$> transformOpts)
  <|> flg ConfluenceTest "confluence-test" "Checks transformation confluence by generating random two pipelines which reaches the fix points."
  <|> flg PrintErrors "print-errors" "Prints the error log"


maybeRenderingOpt :: String -> Maybe RenderingOption
maybeRenderingOpt = M.parseMaybe renderingOpt

renderingOpt :: M.Parsec Void String RenderingOption
renderingOpt = Simple        <$ M.string "simple"
           <|> WithExternals <$ M.string "with-externals"

{- NOTE: Cannot use default in some/many combinators.
   The library considers default value as:
     "nothing is given, then use default",
   but we want it to behave like:
    "if the flag is parsed, and no argument is given, then use default"
-}
printGrinWithOpt :: Parser PipelineStep
printGrinWithOpt = flip PrintGrin id <$> option (maybeReader maybeRenderingOpt)
  ( long "print-grin"
  <> help "Print the actual grin code with a given rendering option [simple | with-externals]"
  <> metavar "OPT" )

options :: [String] -> IO Options
options args = do
  let res = execParserPure defaultPrefs
              (info
                (pipelineArgs <**> helper)
                (mconcat
                  [ fullDesc
                  , progDesc "grin compiler"
                  , header "grin compiler"
                  ]))
              args
  handleParseResult res
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
      <*> switch (mconcat
            [ long "no-prelude"
            , help "Exclude predefined GRIN primops"
            ])
      <*> switch (mconcat
            [ short 'q'
            , long "quiet"
            , help "Quiet mode. Silent pipeline logs"
            ])
      <*> switch (mconcat
            [ long "load-binary"
            , help "Read grin from a binary file"
            ])
      <*> switch (mconcat
            [ long "save-binary-intermed"
            , help "Save intermediate results in binary format"
            ])
      <*> many (strOption (mconcat
            [ short 'C'
            , long "c-file"
            , help "The path for the runtime implementation in C"]))
      <*> switch (mconcat
            [ long "continue-on-failed-lint"
            , help "Do not fail on lint errors"
            ])

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
  hSetBuffering stdout NoBuffering
  Options
    files
    steps
    outputDir
    noPrelude
    quiet
    loadBinary
    saveBinary
    cFiles
    continueOnLint
    <- options args
  forM_ files $ \fname -> do
    (mTypeEnv, program) <- if loadBinary
      then do
        ((,) Nothing . Nametable.restore) <$> Binary.decodeFile fname
      else do
        content <- Text.readFile fname
        let (typeEnv, program') = either (error . M.errorBundlePretty) id $ parseGrinWithTypes fname content
        pure $ (Just typeEnv, if noPrelude then program' else concatPrograms [primPrelude, program'])
    let opts = defaultOpts
                { _poOutputDir = outputDir
                , _poFailOnLint = not continueOnLint
                , _poLogging = not quiet
                , _poSaveBinary = saveBinary
                , _poCFiles = cFiles
                }
    case steps of
      [] -> void $ optimize opts program [] postPipeline
      _  -> void $ pipeline opts mTypeEnv program steps

postPipeline :: [PipelineStep]
postPipeline =
  [ SaveLLVM $ Rel "high-level-opt-code"
  , JITLLVM -- TODO: Remove this.
  , PrintTypeEnv
  , SimplePrintGrin ondullblack
  ]
