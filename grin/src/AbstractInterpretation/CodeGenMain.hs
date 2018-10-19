module AbstractInterpretation.CodeGenMain
  ( Mode(..)
  , codeGen
  ) where

import Control.Monad ( void )

import Grin.Syntax
import AbstractInterpretation.CodeGen
import AbstractInterpretation.Sharing
import AbstractInterpretation.IR ( HPTProgram, Reg )

-- TODO: Remove this module after merging the SharedBy analysis.

codeGen :: Mode -> Exp -> Either String HPTProgram
codeGen mode = codeGenPhases createSharingCtx
  [ hptCodeGen
  , sharingCodeGen mode
  ]

type SharingCtx = Reg

-- | Creates a context for the sharing
createSharingCtx :: CG SharingCtx
createSharingCtx = newReg

