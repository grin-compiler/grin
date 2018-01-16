{-# LANGUAGE DeriveGeneric, LambdaCase, TypeApplications #-}
module Test where

import Prelude hiding (GT)

import Control.Applicative
import Control.Monad
import Control.Monad.Extra (loopM)
import Control.Monad.Logic
import Control.Monad.Trans (lift)
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State -- .Plus
import Control.Monad.Trans.Reader
import qualified Control.Monad.State.Class as CMS
import qualified Control.Monad.Reader.Class as CMR
import Data.Bifunctor
import Data.Functor.Infix
import Data.Functor.Foldable
import Data.List ((\\))
import Data.Maybe (fromJust)
import Data.Semigroup
import GHC.Generics
import Grin
import Test.QuickCheck
import Generic.Random.Generic
import Lens.Micro
import Lens.Micro.Mtl

import Data.Set (Set); import qualified Data.Set as Set
import Data.Map (Map); import qualified Data.Map as Map

import Debug.Trace


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

nonWellFormedPrograms :: Gen Exp
nonWellFormedPrograms = resize 2 (asExp <$> arbitrary @TProg)

instance AsExp TProg where
  asExp = \case
    TProg defs -> Program (asExp <$> defs)

instance AsExp TDef where
  asExp = \case
    TDef name params exp -> Def (toName name) (toName <$> params) (asExp exp)

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


type GoalM a = ReaderT Context (LogicT Gen) a

runGoalM :: GoalM a -> Gen [a]
runGoalM = observeAllT . flip runReaderT mempty

genProg :: Gen [Exp]
genProg = asExp <$$> (runGoalM $ solve @TExp (Exp [] TTUnit))


gen :: Gen a -> GoalM a
gen = lift . lift

newName :: TVal -> (String -> GoalM a) -> GoalM a
newName x k = do
  (Env vars funs) <- view _1
  name <- gen $ (unTName <$> arbitrary) `suchThatIncreases` (`notElem` (Map.keys vars))
  CMR.local (_1 %~ insertVar name (Left x)) $ do
    env1 <- view _1
    k name

gValue :: Type -> GoalM TVal
gValue = \case
  TTUnit          -> pure TUnit
  TInt            -> mzero
  TTLoc           -> mzero
  TTag tag types  -> mzero
  TUnion types    -> mzero

gSExp :: Eff -> GoalM TSExp
gSExp = \case
  NoEff           -> mzero
  NewLoc t        -> TSStore <$> solve (GVal t)
  ReadLoc loc t   -> mzero -- find a name that contains the location and the given type.
  UpdateLoc loc t -> mzero -- fing a name that contains the location and generate  value of a given type

moneof :: [GoalM a] -> GoalM a
moneof [] = mzero
moneof gs = do
  n <- gen $ choose (0, length gs - 1)
  gs !! n

-- TODO: Generate values for effects
-- TODO: Limit exp generation by values
gExp :: Type -> [Eff] -> GoalM TExp
gExp t = \case
  [] -> moneof
    [ (TSExp . TSReturn) <$> solve (GVal t)
    , do x <- solve (GVal t)
         let se = TSReturn x
         newName x $ \n -> do-- TODO: Gen LPat
           rest <- solve (Exp [] t)
           pure (TEBind se (TLPatSVal (TVar (TName n))) rest)
    ]
  es -> mzero

class Solve t where
  solve' :: Goal -> GoalM t

-- TODO: Remove debug...
solve :: Solve t => Goal -> GoalM t
solve g = do
--  env <- view _1
--  traceShowM env
  solve' g

instance Solve TVal where
  solve' = \case
    GVal e -> gValue e
    _      -> mzero

instance Solve TSExp where
  solve' = \case
    SExp e -> gSExp e
    _      -> mzero

instance Solve TExp where
  solve' = \case
    Exp es t -> gExp t es
    _        -> mzero
