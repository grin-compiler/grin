{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Test.Hspec.PipelineExample where

import Grin.Syntax (Program)
import Grin.Parse
import Grin.Pretty (pretty)
import Text.PrettyPrint.ANSI.Leijen (plain)
import Pipeline.Pipeline -- as Grin

import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad (when)
import Control.Monad.Trans
import Data.List (isSuffixOf)
import Data.Text.IO as Text
import System.FilePath ((</>))
import Test.Hspec.Core.Spec hiding (pending)

import System.Directory (getCurrentDirectory)


data Input
  = Src   Program
  | File  FilePath

data Pipeline = Pipeline
  { before :: Input
  , after  :: Input
  , steps  :: [PipelineStep]
  }

pipeline :: FilePath -> FilePath -> [PipelineStep] -> Pipeline
pipeline a b s = Pipeline (File a) (File b) s

pipelineSrc :: Program -> Program -> [PipelineStep] -> Pipeline
pipelineSrc a b s = Pipeline (Src a) (Src b) s

pending :: String -> ResultStatus
pending = Pending Nothing . Just


instance Example Pipeline where
  type Arg Pipeline = ()
  evaluateExample (Pipeline{..}) params actionWith progressCallback = do
    pwd <- getCurrentDirectory
    -- There is a difference between the 'stack ghci --test' and 'stack test'.
    -- Stack test uses the grin/grin meanwhile stack ghci uses 'grin' directory
    let testDataDir = if | "/grin/grin" `isSuffixOf` pwd -> "test-data/"
                         | "/grin"      `isSuffixOf` pwd -> "grin/test-data/"
                         | otherwise -> error "Impossible: stack did not run inside the project dir."
        getInput = \case
          Src prg -> pure prg
          File fn -> parseProg <$> Text.readFile (testDataDir </> fn)

    fmap (Result "" . either id (const Success)) $ runExceptT $ do
      result <- lift $ try $ do
        (,) <$> getInput before
            <*> getInput after
      (beforeExp, afterExp) <- either (throwE . pending . show @SomeException) pure $ result
      let opts = PipelineOpts
            { _poOutputDir = ".output" -- TODO: Random test dir
            , _poFailOnLint = False
            , _poLogging = False
            , _poSaveTypeEnv = False
            , _poStatistics = False
            , _poLintOnChange = False
            , _poTypedLint = False
            }
      resultExp <- lift $ Pipeline.Pipeline.pipeline opts Nothing beforeExp steps
      when (afterExp /= resultExp) $ do
        throwE $ Failure Nothing $ ExpectedButGot Nothing
          (show $ plain $ pretty afterExp)
          (show $ plain $ pretty resultExp)
