{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GhcDump.Pretty
    ( Pretty(..)
    , module GhcDump.Pretty
    ) where

import GhcDump_Ast
import GhcDump.Util

import Data.Ratio
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.ANSI.Leijen

data PrettyOpts = PrettyOpts { showUniques    :: Bool
                             , showIdInfo     :: Bool
                             , showLetTypes   :: Bool
                             , showUnfoldings :: Bool
                             }

defaultPrettyOpts :: PrettyOpts
defaultPrettyOpts = PrettyOpts { showUniques    = False
                               , showIdInfo     = False
                               , showLetTypes   = False
                               , showUnfoldings = False
                               }

-- orphan
instance Pretty T_Text where
    pretty = text . BS.unpack

instance Pretty ExternalName where
    pretty n@ExternalName{} = pretty (externalModuleName n) <> "." <> text (BS.unpack $ externalName n)
    pretty ForeignCall = "<foreign>"

instance Pretty ModuleName where
    pretty = text . BS.unpack . getModuleName

instance Pretty Unique where
    pretty = text . show

instance Pretty BinderId where
    pretty (BinderId b) = pretty b

instance Pretty Binder where
    pretty = pprBinder defaultPrettyOpts

pprBinder :: PrettyOpts -> Binder -> Doc
pprBinder opts b
  | showUniques opts = pretty $ binderUniqueName b
  | otherwise        = pretty $ binderName $ unBndr b

instance Pretty TyCon where
    pretty (TyCon t _) = text $ BS.unpack t

pprRational :: Rational -> Doc
pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance Pretty Lit where
    pretty (MachChar x) = "'" <> char x <> "'#"
    pretty (MachStr x) = "\"" <> text (BS.unpack x) <> "\"#"
    pretty MachNullAddr = "nullAddr#"
    pretty (MachInt x) = pretty x <> "#"
    pretty (MachInt64 x) = pretty x <> "#"
    pretty (MachWord x) = pretty x <> "#"
    pretty (MachWord64 x) = pretty x <> "##"
    pretty (MachFloat x) = "FLOAT" <> parens (pprRational x)
    pretty (MachDouble x) = "DOUBLE" <> parens (pprRational x)
    pretty (MachLabel x) = "LABEL"<> parens (pretty x)
    pretty (LitInteger x) = pretty x

instance Pretty CoreStats where
    pretty c =
        "Core Size"
        <>braces (hsep [ "terms="<>int (csTerms c)
                       , "types="<>int (csTypes c)
                       , "cos="<>int (csCoercions c)
                       , "vbinds="<>int (csValBinds c)
                       , "jbinds="<>int (csJoinBinds c)
                       ])

pprIdInfo :: PrettyOpts -> IdInfo Binder Binder -> IdDetails -> Doc
pprIdInfo opts i d
  | not $ showIdInfo opts = empty
  | otherwise = comment $ "IdInfo:" <+> align doc
  where
    doc = sep $ punctuate ", "
          $ [ pretty d
            , "arity=" <> pretty (idiArity i)
            , "inline=" <> pretty (idiInlinePragma i)
            , "occ=" <> pretty (idiOccInfo i)
            , "str=" <> pretty (idiStrictnessSig i)
            , "dmd=" <> pretty (idiDemandSig i)
            , "call-arity=" <> pretty (idiCallArity i)
            , "unfolding=" <> pprUnfolding opts (idiUnfolding i)
            ] ++ (if idiIsOneShot i then ["one-shot"] else [])

pprUnfolding :: PrettyOpts -> Unfolding Binder Binder -> Doc
pprUnfolding _    NoUnfolding = "NoUnfolding"
pprUnfolding _    BootUnfolding = "BootUnfolding"
pprUnfolding _    OtherCon{} = "OtherCon"
pprUnfolding _    DFunUnfolding = "DFunUnfolding"
pprUnfolding opts CoreUnfolding{..}
  | showUnfoldings opts = "CoreUnf" <> braces
     (align $ sep [ "is-value=" <> pretty unfIsValue
                  , "con-like=" <> pretty unfIsConLike
                  , "work-free=" <> pretty unfIsWorkFree
                  , "guidance=" <> pretty unfGuidance
                  , "template=" <> pprExpr opts unfTemplate
                  ])
  | otherwise = "CoreUnf{..}"

instance Pretty OccInfo where
    pretty OccManyOccs = "Many"
    pretty OccDead = "Dead"
    pretty OccOneOcc = "One"
    pretty (OccLoopBreaker strong) =
        if strong then "Strong Loopbrk" else "Weak Loopbrk"

instance Pretty IdDetails where
    pretty = text . show

data TyPrec   -- See Note [Precedence in types] in TyCoRep.hs
  = TopPrec         -- No parens
  | FunPrec         -- Function args; no parens for tycon apps
  | TyOpPrec        -- Infix operator
  | TyConPrec       -- Tycon args; no parens for atomic
  deriving( Eq, Ord )

pprType :: PrettyOpts -> Type -> Doc
pprType opts = pprType' opts TopPrec

pprType' :: PrettyOpts -> TyPrec -> Type -> Doc
pprType' opts _ (VarTy b)         = pprBinder opts b
pprType' opts p t@(FunTy _ _)     = maybeParens (p >= FunPrec) $ sep $ punctuate " ->" (map (pprType' opts FunPrec) (splitFunTys t))
pprType' opts p (TyConApp tc [])  = pretty tc
pprType' opts p (TyConApp tc tys) = maybeParens (p >= TyConPrec) $ pretty tc <+> hsep (map (pprType' opts TyConPrec) tys)
pprType' opts p (AppTy a b)       = maybeParens (p >= TyConPrec) $ pprType' opts TyConPrec a <+> pprType' opts TyConPrec b
pprType' opts p t@(ForAllTy _ _)  = let (bs, t') = splitForAlls t
                                    in maybeParens (p >= TyOpPrec)
                                       $ "forall" <+> hsep (map (pprBinder opts) bs) <> "." <+> pprType opts t'
pprType' opts _ LitTy             = "LIT"
pprType' opts _ CoercionTy        = "Co"

maybeParens :: Bool -> Doc -> Doc
maybeParens True  = parens
maybeParens False = id

instance Pretty Type where
    pretty = pprType defaultPrettyOpts

pprExpr :: PrettyOpts -> Expr -> Doc
pprExpr opts = pprExpr' opts False

pprExpr' :: PrettyOpts -> Bool -> Expr -> Doc
pprExpr' opts _parens (EVar v)         = pprBinder opts v
pprExpr' opts _parens (EVarGlobal v)   = pretty v
pprExpr' opts _parens (ELit l)         = pretty l
pprExpr' opts parens  e@(EApp{})       = let (x, ys) = collectArgs e
                                         in maybeParens parens $ hang' (pprExpr' opts True x) 2 (sep $ map pprArg ys)
  where pprArg (EType t) = char '@' <> pprType' opts TyConPrec t
        pprArg x         = pprExpr' opts True x
pprExpr' opts parens  x@(ETyLam _ _)   = let (bs, x') = collectTyBinders x
                                         in maybeParens parens
                                            $ hang' ("Λ" <+> sep (map (pprBinder opts) bs) <+> smallRArrow) 2 (pprExpr' opts False x')
pprExpr' opts parens  x@(ELam _ _)     = let (bs, x') = collectBinders x
                                         in maybeParens parens
                                            $ hang' ("λ" <+> sep (map (pprBinder opts) bs) <+> smallRArrow) 2 (pprExpr' opts False x')
pprExpr' opts parens  (ELet xs y)      = maybeParens parens $ "let" <+> (align $ vcat $ map (uncurry (pprBinding opts)) xs)
                                         <$$> "in" <+> align (pprExpr' opts False y)
  where pprBind (b, rhs) = pprBinder opts b <+> equals <+> align (pprExpr' opts False rhs)
pprExpr' opts parens  (ECase x b alts) = maybeParens parens
                                         $ sep [ sep [ "case" <+> pprExpr' opts False x
                                                     , "of" <+> pprBinder opts b <+> "{" ]
                                               , indent 2 $ vcat $ map pprAlt alts
                                               , "}"
                                               ]
  where pprAlt (Alt con bndrs rhs) = hang' (hsep (pretty con : map (pprBinder opts) bndrs) <+> smallRArrow) 2 (pprExpr' opts False rhs)
pprExpr' opts parens  (EType t)        = maybeParens parens $ "TYPE:" <+> pprType opts t
pprExpr' opts parens  ECoercion        = "CO"

instance Pretty AltCon where
    pretty (AltDataCon m t) = maybe mempty (\n -> text (BS.unpack n) <> ".") m <> text (BS.unpack t)
    pretty (AltLit l) = pretty l
    pretty AltDefault = text "DEFAULT"

instance Pretty Expr where
    pretty = pprExpr defaultPrettyOpts

pprTopBinding :: PrettyOpts -> TopBinding -> Doc
pprTopBinding opts tb =
    case tb of
      NonRecTopBinding b s rhs -> pprTopBind (b,s,rhs)
      RecTopBinding bs -> "rec" <+> braces (line <> vsep (map pprTopBind bs))
  where
    pprTopBind (b@(Bndr b'),s,rhs) =
        pprTypeSig opts b
        <$$> pprIdInfo opts (binderIdInfo b') (binderIdDetails b')
        <$$> comment (pretty s)
        <$$> hang' (pprBinder opts b <+> equals) 2 (pprExpr opts rhs)
        <> line

pprTypeSig :: PrettyOpts -> Binder -> Doc
pprTypeSig opts b@(Bndr b') =
    pprBinder opts b <+> dcolon <+> align (pprType opts (binderType b'))

pprBinding :: PrettyOpts -> Binder -> Expr -> Doc
pprBinding opts b@(Bndr b'@Binder{}) rhs =
    ppWhen (showLetTypes opts) (pprTypeSig opts b)
    <$$> pprIdInfo opts (binderIdInfo b') (binderIdDetails b')
    <$$> hang' (pprBinder opts b <+> equals) 2 (pprExpr opts rhs)
pprBinding opts b@(Bndr TyBinder{}) rhs =
    -- let-bound type variables: who knew?
    hang' (pprBinder opts b <+> equals) 2 (pprExpr opts rhs)

instance Pretty TopBinding where
    pretty = pprTopBinding defaultPrettyOpts

pprModule :: PrettyOpts -> Module -> Doc
pprModule opts m =
    comment (pretty $ modulePhase m)
    <$$> text "module" <+> pretty (moduleName m) <+> "where" <> line
    <$$> vsep (map (pprTopBinding opts) (moduleTopBindings m))

instance Pretty Module where
    pretty = pprModule defaultPrettyOpts

comment :: Doc -> Doc
comment x = "{-" <+> x <+> "-}"

dcolon :: Doc
dcolon = "::"

smallRArrow :: Doc
smallRArrow = "→"

hang' :: Doc -> Int -> Doc -> Doc
hang' d1 n d2 = hang n $ sep [d1, d2]

ppWhen :: Bool -> Doc -> Doc
ppWhen True x = x
ppWhen False _ = empty
