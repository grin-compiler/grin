{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
module Test.Hspec.PipelineExample where

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



data Pipeline = Pipeline
  { before :: FilePath
  , after  :: FilePath
  , steps  :: [PipelineStep]
  }

pipeline :: FilePath -> FilePath -> [PipelineStep] -> Pipeline
pipeline = Pipeline

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
    fmap (Result "" . either id (const Success)) $ runExceptT $ do
      result <- lift $ try $ do
        (,) <$> (parseProg <$> Text.readFile (testDataDir </> before))
            <*> (parseProg <$> Text.readFile (testDataDir </> after))
      (beforeExp, afterExp) <- either (throwE . pending . show @SomeException) pure $ result
      let opts = PipelineOpts
            { _poOutputDir = ".output" -- TODO: Random test dir
            , _poFailOnLint = False
            , _poLogging = False
            , _poSaveTypeEnv = False
            , _poStatistics = False
            , _poLintOnChange = False
            }
      resultExp <- lift $ Pipeline.Pipeline.pipeline opts Nothing beforeExp steps
      when (afterExp /= resultExp) $ do
        throwE $ Failure Nothing $ ExpectedButGot Nothing
          (show $ plain $ pretty afterExp)
          (show $ plain $ pretty resultExp)
