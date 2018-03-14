{-# LANGUAGE TemplateHaskell #-}
module GrinTH where

import NeatInterpolation
import qualified ParseGrin as P
import qualified Data.Text as T

import Language.Haskell.TH
import Language.Haskell.TH.Quote

def :: QuasiQuoter
def = text { quoteExp = applyParseDef . quoteExp text }

applyParseDef :: Q Exp -> Q Exp
applyParseDef q = appE [|P.parseDef|] $ appE [|T.unpack|] q

expr :: QuasiQuoter
expr = text { quoteExp = applyParseExpr . quoteExp text }

applyParseExpr :: Q Exp -> Q Exp
applyParseExpr q = appE [|P.parseExpr|] $ appE [|T.unpack|] q
