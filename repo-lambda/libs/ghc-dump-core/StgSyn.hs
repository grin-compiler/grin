-- | A top-level binding.
data GenStgTopBinding bndr occ
-- See Note [CoreSyn top-level string literals]
  = StgTopLifted (GenStgBinding bndr occ)
  | StgTopStringLit bndr ByteString

data GenStgBinding bndr occ
  = StgNonRec bndr (GenStgRhs bndr occ)
  | StgRec    [(bndr, GenStgRhs bndr occ)]

data GenStgArg occ
  = StgVarArg  occ
  | StgLitArg  Literal

data GenStgExpr bndr occ
  = StgApp
        occ             -- function
        [GenStgArg occ] -- arguments; may be empty

  | StgLit      Literal

        -- StgConApp is vital for returning unboxed tuples or sums
        -- which can't be let-bound first
  | StgConApp   DataCon
                [GenStgArg occ] -- Saturated
                [Type]          -- See Note [Types in StgConApp] in UnariseStg

  | StgOpApp    StgOp           -- Primitive op or foreign call
                [GenStgArg occ] -- Saturated.
                Type            -- Result type
                                -- We need to know this so that we can
                                -- assign result registers

  | StgLam
        [bndr]
        StgExpr    -- Body of lambda

  | StgCase
        (GenStgExpr bndr occ)
                    -- the thing to examine

        bndr        -- binds the result of evaluating the scrutinee

        AltType

        [GenStgAlt bndr occ]
                    -- The DEFAULT case is always *first*
                    -- if it is there at all

  | StgLet
        (GenStgBinding bndr occ)    -- right hand sides (see below)
        (GenStgExpr bndr occ)       -- body

  | StgLetNoEscape
        (GenStgBinding bndr occ)    -- right hand sides (see below)
        (GenStgExpr bndr occ)       -- body

  | StgTick
    (Tickish bndr)
    (GenStgExpr bndr occ)       -- sub expression

data GenStgRhs bndr occ
  = StgRhsClosure
        CostCentreStack         -- CCS to be attached (default is CurrentCCS)
        StgBinderInfo           -- Info about how this binder is used (see below)
        [occ]                   -- non-global free vars; a list, rather than
                                -- a set, because order is important
        !UpdateFlag             -- ReEntrant | Updatable | SingleEntry
        [bndr]                  -- arguments; if empty, then not a function;
                                -- as above, order is important.
        (GenStgExpr bndr occ)   -- body

  | StgRhsCon
        CostCentreStack  -- CCS to be attached (default is CurrentCCS).
                         -- Top-level (static) ones will end up with
                         -- DontCareCCS, because we don't count static
                         -- data in heap profiles, and we don't set CCCS
                         -- from static closure.
        DataCon          -- Constructor. Never an unboxed tuple or sum, as those
                         -- are not allocated.
        [GenStgArg occ]  -- Args

data StgBinderInfo
  = NoStgBinderInfo
  | SatCallsOnly        -- All occurrences are *saturated* *function* calls
                        -- This means we don't need to build an info table and
                        -- slow entry code for the thing
                        -- Thunks never get this value

type GenStgAlt bndr occ
  = (AltCon,            -- alts: data constructor,
     [bndr],            -- constructor's parameters,
     GenStgExpr bndr occ)       -- ...right-hand side.

data AltType
  = PolyAlt             -- Polymorphic (a lifted type variable)
  | MultiValAlt Int     -- Multi value of this arity (unboxed tuple or sum)
                        -- the arity could indeed be 1 for unary unboxed tuple
  | AlgAlt      TyCon   -- Algebraic data type; the AltCons will be DataAlts
  | PrimAlt     PrimRep -- Primitive data type; the AltCons (if any) will be LitAlts

data UpdateFlag = ReEntrant | Updatable | SingleEntry

data StgOp
  = StgPrimOp  PrimOp

  | StgPrimCallOp PrimCall

  | StgFCallOp ForeignCall Unique
        -- The Unique is occasionally needed by the C pretty-printer
        -- (which lacks a unique supply), notably when generating a
        -- typedef for foreign-export-dynamic

type StgTopBinding = GenStgTopBinding Id Id
type StgBinding  = GenStgBinding  Id Id
type StgArg      = GenStgArg      Id
type StgExpr     = GenStgExpr     Id Id
type StgRhs      = GenStgRhs      Id Id
type StgAlt      = GenStgAlt      Id Id
