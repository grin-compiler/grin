{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Frontend.Idris.CodegenGrin(codegenGrin) where

import Control.Monad
import Text.Show.Pretty

import IRTS.CodegenCommon
import Grin

codegenGrin :: CodeGenerator
codegenGrin CodegenInfo{..} = do
  forM_ simpleDecls $ \(name, sdecl) -> do
    print name
    putStrLn $ ppShow sdecl
    putStr "\n"
