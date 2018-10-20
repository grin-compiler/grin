module AbstractInterpretation.CodeGenMain
  ( codeGen
  ) where

import Control.Monad ( void )

import Grin.Syntax
import AbstractInterpretation.CodeGen
import AbstractInterpretation.Sharing
import AbstractInterpretation.IR ( HPTProgram, Reg )

-- TODO: Remove this module after merging the SharedBy analysis.

codeGen :: Exp -> Either String HPTProgram
codeGen = codeGenPhases createSharingCtx
  [ hptCodeGen
  , sharingCodeGen
  ]

type SharingCtx = Reg

-- | Creates a context for the sharing
createSharingCtx :: CG SharingCtx
createSharingCtx = newReg

