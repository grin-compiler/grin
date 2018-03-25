{-# LANGUAGE TemplateHaskell #-}
module GrinTH where

import NeatInterpolation
import qualified ParseGrin as P
import qualified Data.Text as T

import Language.Haskell.TH
import Language.Haskell.TH.Quote

prog :: QuasiQuoter
prog = text { quoteExp = applyParseProg . quoteExp text }

applyParseProg:: Q Exp -> Q Exp
applyParseProg q = appE [|P.parseProg|] $ appE [|T.unpack|] q

def :: QuasiQuoter
def = text { quoteExp = applyParseDef . quoteExp text }

applyParseDef :: Q Exp -> Q Exp
applyParseDef q = appE [|P.parseDef|] $ appE [|T.unpack|] q

expr :: QuasiQuoter
expr = text { quoteExp = applyParseExpr . quoteExp text }

applyParseExpr :: Q Exp -> Q Exp
applyParseExpr q = appE [|P.parseExpr|] $ appE [|T.unpack|] q
