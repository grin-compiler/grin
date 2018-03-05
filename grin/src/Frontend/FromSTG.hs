{-# LANGUAGE LambdaCase, TupleSections #-}
module Frontend.FromSTG where

import Text.Printf

-- GHC
import StgSyn
import Id
import Name as GHC
import DynFlags
import Outputable
import Literal
import DataCon

-- Grin
import Grin

import Control.Monad as M
import Control.Monad.State

type CG = State Env

data Env
  = Env
  { dflags  :: DynFlags
  }

getDFlags :: CG DynFlags
getDFlags = gets dflags

genName :: Id -> CG String
genName = undefined

pprM :: Outputable a => a -> CG String
pprM a = flip showPpr a <$> gets dflags

emit = undefined

convertLit :: Literal -> CG Lit
convertLit = \case
  MachInt     i -> pure $ LInt64 $ fromIntegral i
  MachInt64   i -> pure $ LInt64 $ fromIntegral i
  MachWord    w -> pure $ LWord64 $ fromIntegral w
  MachWord64  w -> pure $ LWord64 $ fromIntegral w
  MachFloat   f -> pure $ LFloat $ realToFrac f
  MachDouble  f -> pure $ LFloat $ realToFrac f
  lit -> error . printf "unsupported literal %s" <$> pprM lit

visitArg :: StgArg -> CG Val
visitArg = \case
  StgVarArg id  -> Var <$> genName id
  StgLitArg lit -> Lit <$> convertLit lit

visitRhs :: Id -> StgRhs -> CG ()
visitRhs id rhs = case rhs of
  StgRhsCon _ dataCon args -> pure () -- TODO
  StgRhsClosure _ _ freeVars _ _ args body -> do
    {-
      TODO:
        - add def to globals with the right argumentum list
        - generate the body
    -}
    visitExpr body
    pure ()

visitBinding :: StgBinding -> CG ()
visitBinding = \case
  StgNonRec id stgRhs -> visitRhs id stgRhs
  StgRec bindings     -> mapM_ (uncurry visitRhs) bindings

visitExpr :: StgExpr -> CG ()
visitExpr = \case
  StgApp id args                  -> SApp <$> genName id <*> mapM visitArg args >>= emit
  StgOpApp op args _ty            -> SApp <$> genOpName op <*> mapM visitArg args >>= emit
  StgConApp dataCon args          -> ConstTagNode <$> genTag dataCon <*> mapM visitArg args >>= emit . SReturn
  StgLit literal                  -> SReturn . Lit <$> convertLit literal >>= emit
  StgTick _ expr                  -> visitExpr expr
  StgLet binding expr             -> visitBinding binding >> visitExpr expr -- TODO: generate local or global bind
  StgLetNoEscape _ _ binding expr -> visitBinding binding >> visitExpr expr -- TODO: generate local or global bind
  StgCase expr _ _ result _ _ alts  -> undefined -- TODO: construct case expression
  expr -> error . printf "unsupported expr %s" <$> pprM expr

genOpName :: StgOp -> CG String
genOpName = \case
  StgPrimOp op      -> pprM op -- TODO
  StgPrimCallOp op  -> pprM op -- TODO
  StgFCallOp op _   -> pprM op -- TODO

genTag :: DataCon -> CG Tag
genTag dataCon = Tag C <$> pprM dataCon

{-
-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data AltCon
  = DataAlt DataCon   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                      -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@

  | LitAlt  Literal   -- ^ A literal: @case e of { 1 -> ... }@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]

  | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
   deriving (Eq, Ord, Data, Typeable)

type GenStgAlt bndr occ
  = (AltCon,            -- alts: data constructor,
     [bndr],            -- constructor's parameters,
     [Bool],            -- "use mask", same length as
                        -- parameters; a True in a
                        -- param's position if it is
                        -- used in the ...
     GenStgExpr bndr occ)       -- ...right-hand side.

data DataCon
  = MkData {
        dcName    :: Name,      -- This is the name of the *source data con*
                                -- (see "Note [Data Constructor Naming]" above)
        dcUnique :: Unique,     -- Cached from Name
        dcTag    :: ConTag,     -- ^ Tag, used for ordering 'DataCon's

-}