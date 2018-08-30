module AbstractInterpretation.CodeGenMain
  ( codeGen
  ) where

import Control.Monad ( void )

import Grin.Syntax
import AbstractInterpretation.CodeGen
import AbstractInterpretation.Sharing
import AbstractInterpretation.IR ( HPTProgram )

-- TODO: Remove this module after merging the SharedBy analysis.

codeGen :: Exp -> Either String HPTProgram
codeGen = codeGenPhases
  [ void . hptCodeGen
  , sharingCodeGen UpdatesInEval
  ]
