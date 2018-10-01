module Test.IO where

import System.FilePath

import Grin.Grin
import Grin.Parse

import Test.Hspec


stackRoot :: FilePath
stackRoot = ""

stackSrc :: FilePath
stackSrc = ".."

stackTest :: FilePath
stackTest = "test"

readProgram :: FilePath -> IO Exp
readProgram fp = do
  src <- readFile fp
  return $ parseProg src

withCurDir :: FilePath -> FilePath -> FilePath
withCurDir curDir fp = if curDir == stackTest
  then "." </> fp
  else (curDir </> stackTest) </> fp

runTestsFromWith :: FilePath
                -> (Exp -> a)
                -> [FilePath]
                -> [a -> Spec]
                -> IO ()
runTestsFromWith curDir calcInfo srcs validators = do
  foundResults <- mapM calcInfoIO srcs'
  let validatedResults = zipWith ($) validators foundResults
  mapM_ hspec validatedResults

  where srcs' = map (withCurDir curDir) srcs
        calcInfoIO fp = calcInfo <$> readProgram fp

runBeforeAfterTestsFromWith :: FilePath
                            -> (Exp -> a)
                            -> [FilePath]
                            -> [FilePath]
                            -> [FilePath -> a -> Spec]
                            -> IO ()
runBeforeAfterTestsFromWith curDir calcInfo befores afters validators = do
  foundResults <- mapM calcInfoIO befores'
  let validators'      = zipWith ($) validators  afters'
      validatedResults = zipWith ($) validators' foundResults
  mapM_ hspec validatedResults

  where befores' = map (withCurDir curDir) befores
        afters'  = map (withCurDir curDir) afters
        calcInfoIO fp = calcInfo <$> readProgram fp