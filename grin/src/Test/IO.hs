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

runTestsFromWith :: FilePath
                -> (Exp -> a)
                -> [FilePath]
                -> [a -> Spec]
                -> IO ()
runTestsFromWith fromCurDir calcInfo srcs validators = do
  foundResults <- mapM calcInfoIO srcs'
  let validatedResults = zipWith ($) validators foundResults
  mapM_ hspec validatedResults

  where srcs'         = map ((fromCurDir </> stackTest) </>) srcs
        calcInfoIO fp = calcInfo <$> readProgram fp
