{-# LANGUAGE GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , FlexibleInstances
           , DerivingStrategies
           #-}
module Test.IO where

import System.FilePath

import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)

import Grin.Grin
import Grin.Parse

import Test.Hspec
import Test.Hspec.Core.Spec (SpecM(..))

import Control.Monad.Trans

deriving newtype instance MonadIO (SpecM ())

stackRoot :: FilePath
stackRoot = ""

stackSrc :: FilePath
stackSrc = ".."

stackTest :: FilePath
stackTest = "test"

readProgram :: FilePath -> IO Exp
readProgram = readProgramWith parseProg

readProgramWith :: (Text -> a) -> FilePath -> IO a
readProgramWith parse fp = do
  src <- T.readFile fp
  return $ parse src

withCurDir :: FilePath -> FilePath -> FilePath
withCurDir curDir fp = if curDir == stackTest
  then "." </> fp
  else (curDir </> stackTest) </> fp

testGroup :: String -> Spec -> IO ()
testGroup name tests = hspec $ describe name tests

mkSpecFromWith' :: (Text -> a)
                -> FilePath
                -> (a -> b)
                -> [FilePath]
                -> [b -> Spec]
                -> Spec
mkSpecFromWith' parse curDir calcInfo srcs validators = do
  foundResults <- liftIO $ mapM calcInfoIO srcs'
  let validatedResults = zipWith ($) validators foundResults
  sequence_ validatedResults

  where srcs' = map (withCurDir curDir) srcs
        calcInfoIO fp = calcInfo <$> readProgramWith parse fp

mkSpecFromWith :: FilePath
              -> (Exp -> a)
              -> [FilePath]
              -> [a -> Spec]
              -> Spec
mkSpecFromWith = mkSpecFromWith' parseProg

mkBeforeAfterSpecFrom :: FilePath
                      -> (Exp -> a)
                      -> [FilePath]
                      -> [FilePath]
                      -> [FilePath -> a -> Spec]
                      -> Spec
mkBeforeAfterSpecFrom curDir calcInfo befores afters validators = do
  foundResults <- liftIO $ mapM calcInfoIO befores'
  let validators'      = zipWith ($) validators  afters'
      validatedResults = zipWith ($) validators' foundResults
  sequence_ validatedResults

  where befores' = map (withCurDir curDir) befores
        afters'  = map (withCurDir curDir) afters
        calcInfoIO fp = calcInfo <$> readProgram fp
