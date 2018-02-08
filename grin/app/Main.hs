{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

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
  <|> flg BindNormalisation "bi" "Bind Normalisation"
  <|> flg RightHoistFetch "rhi" "Right Hoist Fetch"
  <|> flg GenerateEval "ge" "Generate Eval"
  <|> flg ConstantFolding "cfl" "Constant Folding"

pipelineOpts :: Parser Pipeline
pipelineOpts =
      flg (HPT CompileHPT) "compile-hpt" "Compiles heap-points-to analysis machine"
  <|> flg (HPT PrintHPT) "print-hpt" "Prints the heap-points-to analysis machine"
  <|> flg (HPT RunHPTPure) "run-hpt-pure" "Runs the heap-points-to analysis machine via pure interpreter"
  <|> flg (HPT PrintHPTResult) "print-hpt-result" "Prints the heap-points-to analysis result"
  <|> flg TagInfo "tag-info" "Tag Information"
  <|> flg (PrintGrin id) "print-grin" "Prints the actual grin code"
  <|> flg PureEval "eval" "Evaluate the grin program"
  <|> flg JITLLVM "llvm" "JIT with LLVM"
  <|> flg PrintAST "ast" "Print the Abstract Syntax Tree"
  <|> (SaveLLVM <$> (strOption (mconcat
        [ long "save-llvm"
        , help "Save the generated llvm"
        ])))
  <|> (SaveLLVM <$> (strOption (mconcat
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
      , HPT RunHPTPure
      , HPT PrintHPTResult
      , T CaseSimplification
      , T Vectorisation
      , T RegisterIntroduction
      , T RightHoistFetch
      , T SplitFetch
      , T BindNormalisation
      , PrintGrin ondullcyan
      , SaveLLVM "code"
      , JITLLVM
      ]
      output
  opts -> opts

main :: IO ()
main = do
  Options files steps outputDir <- defaultPipeline <$> options
  forM_ files $ \fname -> do
    grin <- either (error . show) id <$> parseGrin fname
    let program = Program grin
        opts = PipelineOpts { _poOutputDir = outputDir }
    pipeline opts program steps
