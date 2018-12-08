{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Test.Hspec.PipelineExample where

import Grin.Parse
import Grin.Pretty (pretty)
import Text.PrettyPrint.ANSI.Leijen (plain)
import qualified Pipeline.Definitions as Grin
import qualified Pipeline.Pipeline as Grin

import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad (when)
import Control.Monad.Trans
import Data.Text.IO as Text
import Test.Hspec.Core.Spec hiding (pending)



data Pipeline = Pipeline
  { before :: FilePath
  , after  :: FilePath
  , steps  :: [Grin.PipelineStep]
  }

pipeline :: FilePath -> FilePath -> [Grin.PipelineStep] -> Pipeline
pipeline = Pipeline

pending :: String -> ResultStatus
pending = Pending Nothing . Just

instance Example Pipeline where
  type Arg Pipeline = ()
  evaluateExample (Pipeline{..}) params actionWith progressCallback = do
    fmap (Result "" . either id (const Success)) $ runExceptT $ do
      result <- lift $ try $ do
        (,) <$> (parseProg <$> Text.readFile before)
            <*> (parseProg <$> Text.readFile after)
      (beforeExp, afterExp) <- either (throwE . pending . show @SomeException) pure $ result
      let opts = Grin.PipelineOpts
            { _poOutputDir = ".output" -- TODO: Random test dir
            , _poFailOnLint = False
            , _poLogging = False
            , _poSaveTypeEnv = False
            , _poStatistics = False
            }
      ((), resultExp) <- lift $ Grin.runPipeline opts Nothing beforeExp $ sequence_ $ map Grin.pipelineStep steps
      when (afterExp /= resultExp) $ do
        throwE $ Failure Nothing $ ExpectedButGot Nothing
          (show $ plain $ pretty afterExp)
          (show $ plain $ pretty resultExp)
