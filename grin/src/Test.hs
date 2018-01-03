{-# LANGUAGE DeriveGeneric, TypeFamilies, LambdaCase, TypeApplications #-}
module Test where

import Control.Monad.Extra (loopM)
import Data.Bifunctor
import Data.Functor.Infix
import Data.Functor.Foldable
import Data.List ((\\))
import Data.Semigroup
import GHC.Generics
import Grin
import Test.QuickCheck
import Generic.Random.Generic

import Data.Set (Set); import qualified Data.Set as Set
import Data.Map (Map); import qualified Data.Map as Map


data TName = TName { unTName :: String }
  deriving (Eq, Generic, Show)

data TProg = TProg [TDef]
  deriving (Generic, Show)

data TDef = TDef TName [TName] TExp
  deriving (Generic, Show)

data TExp
  = TEBind TSExp TLPat TExp
  | TECase TVal [TAlt]
  | TSExp TSExp
  deriving (Generic, Show)

data TAlt = TAlt CPat TExp
  deriving (Generic, Show)

data TSExp
  = TSApp     TName [TSimpleVal]
  | TSReturn  TVal
  | TSStore   TVal
  | TSFetchI  TName (Maybe Int)
  | TSUpdate  TName TVal
  | TSBlock   TExp
  deriving (Generic, Show)

data TVal
  = TConstTagNode  Tag   [TSimpleVal]
  | TVarTagNode    TName [TSimpleVal]
  | TValTag        Tag
  | TUnit
  | TSimpleVal TSimpleVal
  deriving (Eq, Generic, Show)

data TSimpleVal
  = TLit Lit
  | TVar TName
  deriving (Eq, Generic, Show)

data TLPat
  = TLPatVal  TVal
  | TLPatSVal TSimpleVal
  deriving (Generic, Show)

type Loc = Int

data TExtraVal
  = TLoc Loc
  deriving (Eq, Generic, Show)


toName (TName n) = n

class AsVal t where
  asVal :: t -> Val

instance AsVal TVal where
  asVal = \case
    TConstTagNode  tag  simpleVals -> ConstTagNode tag (asVal <$> simpleVals)
    TVarTagNode    name simpleVals -> VarTagNode (toName name) (asVal <$> simpleVals)
    TValTag        tag             -> ValTag tag
    TUnit                          -> Unit
    TSimpleVal     simpleVal       -> asVal simpleVal

instance AsVal TSimpleVal where
  asVal = \case
    TLit lit  -> Lit lit
    TVar name -> Var (toName name)

instance AsVal TLPat where
  asVal = \case
    TLPatVal  val  -> asVal val
    TLPatSVal sval -> asVal sval

class AsExp t where
  asExp :: t -> Exp

instance AsExp TSExp where
  asExp = \case
    TSApp     name simpleVals -> SApp (toName name) (asVal <$> simpleVals)
    TSReturn  val -> SReturn (asVal val)
    TSStore   val -> SStore (asVal val)
    TSFetchI  name pos -> SFetchI (toName name) pos
    TSUpdate  name val -> SUpdate (toName name) (asVal val)
    TSBlock   exp -> SBlock (asExp exp)

instance AsExp TExp where
  asExp = \case
    TEBind sexp lpat exp -> EBind (asExp sexp) (asVal lpat) (asExp exp)
    TECase val alts      -> ECase (asVal val) (asExp <$> alts)
    TSExp sexp           -> asExp sexp

instance AsExp TAlt where
  asExp = \case
    TAlt cpat exp -> Alt cpat (asExp exp)



instance Arbitrary TProg where arbitrary = genericArbitraryU
instance Arbitrary TDef where arbitrary = genericArbitraryU
instance Arbitrary TExp where arbitrary = genericArbitraryU
instance Arbitrary TSExp where arbitrary = genericArbitraryU
instance Arbitrary TAlt where arbitrary = genericArbitraryU
instance Arbitrary Val where arbitrary = genericArbitraryU
instance Arbitrary Lit where arbitrary = genericArbitraryU
instance Arbitrary TagType where arbitrary = genericArbitraryU
instance Arbitrary TVal where arbitrary = genericArbitraryU
instance Arbitrary TSimpleVal where arbitrary = genericArbitraryU
instance Arbitrary TExtraVal where arbitrary = genericArbitraryU
instance Arbitrary TLPat where arbitrary = genericArbitraryU

instance Arbitrary CPat where
  arbitrary = oneof
    [ NodePat <$> arbitrary <*> (unTName <$$> listOf1 arbitrary)
    , TagPat  <$> arbitrary
    , LitPat  <$> arbitrary
    ]

instance Arbitrary Tag where
  arbitrary = Tag
    <$> arbitrary
    <*> (unTName <$> arbitrary)
    <*> (getPositive <$> arbitrary)

instance Arbitrary TName where
  arbitrary = TName . concat <$> listOf1 hiragana

-- | Increase the size parameter until the generator succeds.
suchThatIncreases :: Gen a -> (a -> Bool) -> Gen a
suchThatIncreases g p =
  flip loopM 1 $ \s -> do
    resize s
      $ fmap (maybe (Left (s+1)) Right)
      $ suchThatMaybe g p

hiragana :: Gen String
hiragana = elements $
  ( [ c ++ v
    | v <- ["a",  "e",  "i",  "o",  "u"]
    , c <- ["", "k", "s", "t", "n", "h", "m", "y", "r", "w"]
    ] \\ ["yi", "ye", "wu"]) ++ ["n"]

data Env
  = Env
    { vars :: Map Name Type
    , funs :: Map Name ([Type], Type, [Eff])
    }
  deriving (Eq, Show)

instance Monoid Env where
  mempty = Env mempty mempty
  mappend (Env v0 f0) (Env v1 f1) = Env (Map.unionWith (<>) v0 v1) (f0 <> f1)

insertVar :: Name -> Either TVal TExtraVal -> Env -> Env
insertVar name val (Env vars funs) = Env (Map.singleton name (typeOf val) <> vars) funs

data Store = Store (Map Loc (TVal, Type))
  deriving (Eq, Show)

instance Monoid Store where
  mempty = Store mempty
  mappend (Store s0) (Store s1) = Store (s0 <> s1)

data Eff
  = NoEff
  | NewLoc Type
  | ReadLoc Loc Type
  | UpdateLoc Loc Type
  deriving (Eq, Ord, Show)

data Type
  = TTUnit
  | TInt
  | TTLoc
  | TTag Tag [Type]
  | TUnion (Set Type)
  deriving (Eq, Ord, Show)

instance Semigroup Type where
  (TUnion as) <> (TUnion bs) = TUnion (as `Set.union` bs)
  (TUnion as) <> a = TUnion (Set.insert a as)
  a <> (TUnion as) = TUnion (Set.insert a as)
  a <> b = TUnion $ Set.fromList [a,b]

class TypeOf t where
  typeOf :: t -> Type

instance TypeOf TSimpleVal where
  typeOf = \case
    TLit (LInt _) -> TInt
    bad           -> error $ "typeOf @TSimpleVal got:" ++ show bad

instance TypeOf TVal where
  typeOf = \case
    TConstTagNode  tag vals -> TTag tag (typeOf <$> vals)
    TValTag        tag      -> TTag tag []
    TUnit                   -> TTUnit
    TSimpleVal val          -> typeOf val
    bad -> error $ "typeOf got:" ++ show bad

instance TypeOf TExtraVal where
  typeOf = \case
    TLoc _ -> TTLoc

instance (TypeOf l, TypeOf r) => TypeOf (Either l r) where
  typeOf = either typeOf typeOf

type Context = (Env, Store)


data Goal
  = Exp [Eff] Type
  | SExp Eff
  | GVal Type
  deriving (Eq, Ord, Show)

-- TODO: Generate values for types
gValue :: Context -> Type -> Gen (Maybe (TVal, Context))
gValue ctx = (fmap . fmap) addCtx . \case
  TTUnit          -> pure $ pure TUnit
  TInt            -> pure Nothing
  TTLoc           -> pure Nothing
  TTag tag types  -> pure Nothing
  TUnion types    -> pure Nothing
  where
    addCtx x = (x, ctx)

-- TODO: Generate Simple expression for values and updates.
gSExp :: Context -> Eff -> Gen (Maybe (TSExp, Context))
gSExp ctx e = case e of
  NoEff           -> pure Nothing
  NewLoc t        -> first TSStore <$$> solve ctx (GVal t)
  ReadLoc loc t   -> pure Nothing -- find a name that contains the location and the given type.
  UpdateLoc loc t -> pure Nothing -- fing a name that contains the location and generate  value of a given type
  where
    addCtx x = (x, ctx)

newName :: Context -> TVal -> Gen (String, Context)
newName (env, str) x = do
  let Env vars funs = env
  name <- (unTName <$> arbitrary) `suchThatIncreases` (`notElem` (Map.keys vars))
  return (name, ((insertVar name (Left x) env), str))

-- TODO: Generate values for effects
-- TODO: Limit exp generation by values
gExp :: Context -> Type -> [Eff] -> Gen (Maybe (TExp, Context))
gExp ctx t = \case
  [] -> oneof
    [ first (TSExp . TSReturn) <$$> solve ctx (GVal t)
    , do Just (x, ctx0)    <- solve ctx (GVal t)
         let se = TSReturn x
         (n, ctx1)         <- newName ctx0 x -- TODO: Gen LPat
         Just (rest, ctx2) <- solve ctx1 (Exp [] t)
         pure . Just $ (TEBind se (TLPatSVal (TVar (TName n))) rest, ctx2)
    ]
  es -> pure Nothing

class Solve t where
  solve :: Context -> Goal -> Gen (Maybe (t, Context))

instance Solve TVal where
  solve ctx = \case
    GVal t -> gValue ctx t
    bad    -> pure Nothing

instance Solve TSExp where
  solve ctx = \case
    SExp e -> gSExp ctx e
    bad    -> return Nothing

instance Solve TExp where
  solve ctx = \case
    Exp effs t -> gExp ctx t effs
    bad        -> return Nothing

-- TODO: Generate real programs, not just expressions.
genProg :: Gen (Maybe (Exp, Context))
genProg = first (asExp @TExp) <$$> solve mempty (Exp [] TTUnit)
