{-# LANGUAGE LambdaCase, BangPatterns #-}
module Test.Check where

import AbstractInterpretation.HeapPointsTo.Result
import Control.Arrow
import Data.Functor.Foldable
import Data.Maybe
import Data.Monoid
import Grin.Grin

import Data.List       as List
import Data.Map.Strict as Map
import Data.Set        as Set hiding (fold)
import qualified Data.Foldable as Foldable


data Check
  = EveryNameIsDefined
  | OnlyStoreVars
  | OnlyBasicValuesInCases
  | OnlyTagsInAlts
  | OnlyUniqueNames
  | SimpleExpOnLHS
  deriving (Enum, Eq, Show)

data Result
  = Ok
  | Failed String
  deriving (Eq, Show)

isOk :: Result -> Bool
isOk = \case
  Ok       -> True
  Failed _ -> False

allChecks :: Maybe HPTResult -> Exp -> [(Check, Result)]
allChecks hpt = checks hpt . enumFrom $ toEnum 0

checks :: Maybe HPTResult -> [Check] -> Exp -> [(Check, Result)]
checks hpt cs e = zipWith (\c e -> (c, check hpt c e)) cs (repeat e)

check :: Maybe HPTResult -> Check -> Exp -> Result
check hpt = \case
  EveryNameIsDefined     -> result . nonDefinedNames
  OnlyStoreVars          -> result . storedConstants
  OnlyBasicValuesInCases -> boolResult . getAll . valuesInCases (All . isBasicValue)
  OnlyTagsInAlts         -> boolResult . getAll . patsInAlts    (All . isBasicCPat)
  OnlyUniqueNames        -> result . nonUniqueNames
--  AllowedBindStoreValues -> boolResult . allowedBindStoreValues
  SimpleExpOnLHS         -> boolResult . simpleExpOnLHS

result :: Show a => [a] -> Result
result = \case
  [] -> Ok
  as -> Failed $ show as

boolResult :: Bool -> Result
boolResult = \case
  True  -> Ok
  False -> Failed ""

bindStoreValues :: Monoid m => (Val -> m) -> Exp -> m
bindStoreValues f = para $ \case
  EBindF ((SStore _), l) val (_, r) -> mconcat [f val, l, r]
  rest                              -> Foldable.foldMap snd rest

-- | Names introduced by fetch def and patterns.
definedNames :: Monoid m => (Name -> m) -> ExpF m -> m
definedNames f = \case
  DefF defName params names -> mconcat
    [ f defName
    , mconcat (f <$> params)
    , names
    ]
  -- Exp
  EBindF simpleNames lpat restNames -> mconcat
    [ simpleNames
    , restNames
    , foldNames f lpat -- Names in lpat are always new names
    ]
  -- Simple Expr
  SFetchIF  name pos -> f name
  SBlockF   names    -> names
  -- Alt
  AltF cpat names -> mconcat
    [ names
    , foldNames f cpat -- Names in cpat are always new names
    ]
  rest -> Foldable.fold rest

-- | Names that are used in the entire program.
usedNames :: Monoid m => (Name -> m) -> ExpF m -> m
usedNames f = \case
  -- Exp
  EBindF    simpleNames lpat restNames -> mconcat
    [ simpleNames
    , restNames
    ]
  ECaseF valName rest -> mconcat ((foldNames f valName):rest)
  -- Simple Expr
  SAppF     name valNames -> mconcat [f name, mconcat (foldNames f <$> valNames)]
  SReturnF  valName -> foldNames f valName
  SStoreF   valName -> foldNames f valName
  SUpdateF  name val -> f name
  rest -> Foldable.fold rest

nonUniqueNames :: Exp -> [Name]
nonUniqueNames = Map.keys . Map.filter ((> 1) . getSum) . cata (definedNames insert) where
  insert n = Map.singleton n (Sum 1)

nonDefinedNames :: Exp -> [Name]
nonDefinedNames e = Set.toList $ Prelude.foldl Set.difference
  (cata (usedNames Set.singleton) e)
  [ (cata (definedNames Set.singleton) e)
  , Set.fromList $ fmap eName $ externals e
  ]

storedValues :: Monoid m => (Val -> m) -> ExpF m -> m
storedValues f = \case
  SStoreF val -> f val
  rest        -> Foldable.fold rest

storedConstants :: Exp -> [Val]
storedConstants = cata (storedValues (\v -> [v | isConstant v]))

valuesInCases :: (Monoid m) => (Val -> m) -> Exp -> m
valuesInCases f = cata $ \case
  ECaseF val alts -> mconcat (f val:alts)
  rest            -> Foldable.fold rest

patsInAlts :: (Monoid m) => (CPat -> m) -> Exp -> m
patsInAlts f = cata $ \case
  AltF pat val -> (f pat <> val)
  rest         -> Foldable.fold rest

programSize :: Exp -> Int
programSize = cata $ \case
  ProgramF  _ ds    -> sum ds
  DefF      _ _ !a  -> 1 + a
  EBindF    !a _ !b -> sum [1,a,b]
  ECaseF    _ !as   -> sum (1:as)
  SAppF     _ _     -> 1
  SReturnF  _       -> 1
  SStoreF   _       -> 1
  SFetchIF  _ _     -> 1
  SUpdateF  _ _     -> 1
  SBlockF   !a      -> 1 + a
  AltF _ !a         -> 1 + a

simpleExpOnLHS :: Exp -> Bool
simpleExpOnLHS = para $ \case
  ProgramF  _ ds    -> and $ fmap snd ds
  DefF      _ _ a   -> snd a
  EBindF    (s, a) _ b -> and [isSimpleExp s, a, snd b]
  ECaseF    _ as    -> and $ fmap snd as
  SBlockF   a       -> snd a
  AltF _ a          -> snd a
  _                 -> False
