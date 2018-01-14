{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module Main where

import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import AbstractRunGrin
import Eval
import Grin
import ParseGrin hiding (value)
import Pipeline
import Pretty
import PrettyHPT
import System.Process
import TrafoPlayground
import Transformations
import VarGen

import qualified CodeGenLLVM as CGLLVM
import qualified CodeGenX64 as CGX64
import qualified JITLLVM

import Data.IntMap as IntMap
import Data.Map as Map
import LLVM.Pretty (ppllvm)
import System.FilePath

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Set
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Options.Applicative

import qualified Text.Show.Pretty as PS
import qualified Data.Text.Lazy.IO as Text



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
  <|> flg CopyPropagationLeft "cpl" "Copy Propagation Left"
  <|> ((RenameVariables . Map.fromList)
        <$> option auto (mconcat
              [ long "rv"
              , help "RenameVariables"
              ]))

pipelineOpts :: Parser Pipeline
pipelineOpts =
      flg HPT "hpt" "Heap to point analysis"
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
      [ HPT
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
    grin <- either (fail . show) id <$> parseGrin fname
    let program = Program grin
    let result = [printf "stores %s %d" name $ countStores exp | Def name _ exp <- grin]
    putStrLn "* store count *"
    putStrLn $ unlines result
    putStrLn "* tag info *"
    putStrLn . show . collectTagInfo $ program
    let opts = PipelineOpts { _poOutputDir = outputDir }
    pipeline opts program steps
