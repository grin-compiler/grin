{-# LANGUAGE LambdaCase #-}
module Free where

import Control.Monad.State
import Control.Monad.Free
import Grin



type ExpM a = Free ExpF a

program :: [ExpM ()] -> ExpM ()
program defs = Free (ProgramF defs)

def :: Name -> [Name] -> ExpM () -> ExpM ()
def name params body = Free (DefF name params body)

block :: ExpM () -> ExpM ()
block body = Free (SBlockF body)

bind :: ExpM () -> LPat -> ExpM () -> ExpM ()
bind simple pat rest = Free (EBindF simple pat rest)

switch :: Val -> [(CPat, ExpM ())] -> ExpM ()
switch val branches = Free (ECaseF val ((\(cpat, body) -> (Free (AltF cpat body))) <$> branches))

app :: Name -> [SimpleVal] -> ExpM ()
app name params = liftF $ SAppF name params

ret :: Val -> ExpM ()
ret = liftF . SReturnF

store :: Val -> ExpM ()
store = liftF . SStoreF

fetch :: Name -> Maybe Int -> ExpM ()
fetch name pos = liftF $ SFetchIF name pos

update :: Name -> Val -> ExpM ()
update name val = liftF $ SUpdateF name val

build :: ExpM () -> [Exp]
build e = execState (iterM compute e) []
  where
    compute = \case
      ProgramF  defs -> do
        sequence defs
        modify (pure . Program . reverse . take (length defs))

      DefF      name names body -> do
        body
        modify (\(b:rest) -> (Def name names b):rest)

      -- Exp
      EBindF    simple lpat rest -> do
        simple
        rest
        modify (\(r:s:rest) -> (EBind s lpat r):rest)

      ECaseF    val alts -> do
        sequence alts
        modify (\rest -> let (as,rest') = splitAt (length alts) rest in (ECase val (reverse as):rest'))

      -- Simple Expr
      SAppF     name simpleVals -> modify (SApp name simpleVals:)
      SReturnF  val -> modify (SReturn val:)
      SStoreF   val -> modify (SStore val:)
      SFetchIF  name pos -> modify (SFetchI name pos:)
      SUpdateF  name val -> modify (SUpdate name val:)

      SBlockF   exp -> do
        exp
        modify (\(e:r) -> (SBlock e:r))
      -- Alt

      AltF cpat body -> do
        body
        modify (\(b:r) -> (Alt cpat b:r))
