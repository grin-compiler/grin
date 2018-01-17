{-# LANGUAGE LambdaCase #-}
module Check where

import Data.Functor.Foldable
import Data.Monoid
import Grin

import Data.Map.Strict as Map
import Data.Set        as Set


-- | Names introduced by fetch def and patterns.
definedNames :: Monoid m => (Name -> m) -> ExpF m -> m
definedNames f = \case
  ProgramF names -> mconcat names
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
  ECaseF    val names -> mconcat names
  -- Simple Expr
  SAppF     name valNames -> mempty -- Application does not introduce names
  SReturnF  valName -> mempty
  SStoreF   valName -> mempty
  SFetchIF  name pos -> f name
  SUpdateF  name val -> mempty
  SBlockF   names    -> names
  -- Alt
  AltF cpat names -> mconcat
    [ names
    , foldNames f cpat -- Names in cpat are always new names
    ]

-- | Names that are used in the entire program.
usedNames :: Monoid m => (Name -> m) -> ExpF m -> m
usedNames f = \case
  ProgramF  names -> mconcat names
  DefF      defName params names -> names
  -- Exp
  EBindF    simpleNames lpat restNames -> mconcat
    [ simpleNames
    , restNames
    ]
  ECaseF    val names -> mconcat names
  -- Simple Expr
  SAppF     name valNames -> mconcat [f name, mconcat (foldNames f <$> valNames)]
  SReturnF  valName -> foldNames f valName
  SStoreF   valName -> foldNames f valName
  SFetchIF  name pos -> mempty
  SUpdateF  name val -> f name
  SBlockF   names    -> names
  -- Alt
  AltF cpat names -> names

nonUniqueNames :: Exp -> [Name]
nonUniqueNames = Map.keys . Map.filter ((> 1) . getSum) . cata (definedNames insert) where
  insert n = Map.singleton n (Sum 1)

nonDefinedNames :: Exp -> [Name]
nonDefinedNames e = Set.toList $ Set.difference
  (cata (usedNames Set.singleton) e)
  (cata (definedNames Set.singleton) e)

storedValues :: Monoid m => (Val -> m) -> ExpF m -> m
storedValues f = \case
  ProgramF vals      -> mconcat vals
  DefF _ _ val       -> val
  EBindF val1 _ val2 -> val1 <> val2
  SStoreF val        -> f val
  SBlockF val        -> val
  AltF _ val         -> val
  _                  -> mempty

storedConstants :: Exp -> [Val]
storedConstants = cata (storedValues (\v -> [v | isConstant v]))
