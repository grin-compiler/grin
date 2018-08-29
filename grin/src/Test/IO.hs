module Test.IO where

import System.FilePath

import Grin.Grin
import Grin.Parse

readProgram :: FilePath -> IO Exp
readProgram fp = do
  src <- readFile fp
  return $ parseProg src
