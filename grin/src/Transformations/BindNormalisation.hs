{-# LANGUAGE LambdaCase #-}
module Transformations.BindNormalisation where

import Data.Functor.Foldable as Foldable

import Grin

-- Bind normalisation (EXTREMELY UGLY first version, REFACTORING NEEDED!)
bindNormalisation :: Exp -> Exp
bindNormalisation = ($ id) . snd . cata folder where
  folder :: ExpF (Bool, (Exp -> Exp) -> Exp) -> (Bool, (Exp -> Exp) -> Exp)
  folder = \case

    EBindF (hasSBlock, sexpf) pat (_, expf) -> case hasSBlock of
      True  -> (False, \f -> sexpf $ \sexp -> EBind sexp pat (expf f))
      False -> (False, \f -> EBind (sexpf id) pat (expf f))

    SBlockF (_, f) -> (True, f)
    -- SimpleExp: return, app, case, store, fetch, update
    SAppF name vals -> (False, \f -> f (SApp name vals))
    SReturnF val -> (False, \f -> f (SReturn val))
    SStoreF val -> (False, \f -> f (SStore val))
    SFetchIF name index -> (False, \f -> f (SFetchI name index))
    SUpdateF name val -> (False, \f -> f (SUpdate name val))
    AltF cpat (_, expf) -> (False, \f -> f (Alt cpat (expf id)))
    ECaseF val altsf -> (False, \f -> f (ECase val (map (($ id) . snd) altsf)))
    DefF name args (_, expf) -> (False, \f -> f (Def name args (expf id)))
    ProgramF defs -> (False, \f -> f (Program (map (($ id) . snd) defs)))
