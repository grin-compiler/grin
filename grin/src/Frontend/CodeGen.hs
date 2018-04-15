{-# LANGUAGE LambdaCase, TupleSections #-}
module Frontend.CodeGen (codegenGrin) where

import Frontend.Lambda
import qualified Grin as G

-- lazy context
genC = undefined

-- strict context (evaluates to WHNF) ; R is similar to E
genE = undefined
genR = undefined

{-
  requirements:
    arity info for all global functions
-}

{-
  App   C E
  Case  R
  Let   R
  LetS  R
  Con   R
  Var   E
  Lit
  Alt   R
-}

codegenGrin _ = G.Program [] -- TODO