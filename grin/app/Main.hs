{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module Main where

import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Eval
import ParseGrin hiding (value)
import Grin
import Pretty
import PrettyHPT
import Transformations
import TrafoPlayground
import AbstractRunGrin
import qualified CodeGenX64 as CGX64
import qualified CodeGenLLVM as CGLLVM
import qualified JITLLVM
import VarGen
import System.Process

import Data.IntMap as IntMap
import Data.Map as Map
import qualified Text.Show.Pretty as PS
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as Text
import System.FilePath

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.Set
import Options.Applicative

import Pipeline

data Options = Options
  { optFiles :: [FilePath]
  , optTrans :: [Pipeline]
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

defaultPipeline :: Options -> Options
defaultPipeline = \case
  Options files [] ->
    Options files
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
  opts -> opts

main :: IO ()
main = do
  Options files steps <- defaultPipeline <$> options
  forM_ files $ \fname -> do
    grin <- either (fail . show) id <$> parseGrin fname
    let program = Program grin
    let result = [printf "stores %s %d" name $ countStores exp | Def name _ exp <- grin]
    putStrLn "* store count *"
    putStrLn $ unlines result
    putStrLn "* tag info *"
    putStrLn . show . collectTagInfo $ program
    pipeline program steps
