{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module Main where

import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Eval
import ParseGrin
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

import Pipeline


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: grin GRIN_SOURCE"
    x -> forM_ x $ \fname -> do
      grin <- either (fail . show) id <$> parseGrin fname
      let program = Program grin
      let result = [printf "stores %s %d" name $ countStores exp | Def name _ exp <- grin]
      putStrLn "* store count *"
      putStrLn $ unlines result
      putStrLn "* tag info *"
      putStrLn . show . collectTagInfo $ program

      let (result, hptResult) = abstractRun (assignStoreIDs $ Program grin) "grinMain"
      putStrLn "* HPT *"
      print . pretty $ hptResult

      pipeline program
        [ HPT
        , T Vectorisation
        , T SplitFetch
        , T CaseSimplification
        , PrintGrin ondullblack
        ]

      pipeline program
        [ HPT
        , T RegisterIntroduction
        , T $ RenameVariables (Map.fromList [("i'", "i''"), ("a", "a'")])
        , T GenerateEval
        , PrintGrin ondullmagenta
        ]

      pipeline program
        [ HPT, T RegisterIntroduction, PrintGrin ondullred ]

      pipeline program
        [ HPT, T RegisterIntroduction, T BindNormalisation, PrintGrin ondullcyan ]

      pipeline program
        [ PrintGrin id ]

      pipeline program
        [ PureEval ]

      pipeline program
        [ HPT, T RightHoistFetch, PrintGrin id ]

      pipeline program
        [ HPT
        , T CaseSimplification
        , T Vectorisation
        , T RegisterIntroduction
        , T RightHoistFetch
        , T SplitFetch
        , T BindNormalisation
        , PrintGrin ondullcyan
        , JITLLVM
        , SaveLLVM fname
        ]
