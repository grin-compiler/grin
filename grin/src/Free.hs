module Free where

import Control.Monad.Free
import Grin hiding (Exp)


type Exp a = Free ExpF a

program :: [Exp ()] -> Exp ()
program defs = Free (ProgramF defs)

def :: Name -> [Name] -> Exp ()
def name params = liftF $ DefF name params ()

block :: Exp () -> Exp ()
block body = Free (SBlockF body)

bind :: Exp () -> LPat -> Exp () -> Exp ()
bind simple pat rest = Free (EBindF simple pat rest)

switch :: Val -> [(CPat, Exp ())] -> Exp ()
switch val branches = Free (ECaseF val ((\(cpat, body) -> (Free (AltF cpat body))) <$> branches))

app :: Name -> [SimpleVal] -> Exp ()
app name params = liftF $ SAppF name params

ret :: Val -> Exp ()
ret = liftF . SReturnF

store :: Val -> Exp ()
store = liftF . SStoreF

fetch :: Name -> Maybe Int -> Exp ()
fetch name pos = liftF $ SFetchIF name pos

update :: Name -> Val -> Exp ()
update name val = liftF $ SUpdateF name val

