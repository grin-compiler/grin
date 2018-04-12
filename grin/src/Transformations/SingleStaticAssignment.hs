{-# LANGUAGE LambdaCase #-}
module Transformations.SingleStaticAssignment where

import Grin
import Data.Functor.Foldable
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test
import GrinTH
import Test.Hspec
import Assertions

-- Simple implementation for single-static-assignment. It can reuse
-- the same name if it occurs in different blocks. See the test case
singleStaticAssignment :: Exp -> Exp
singleStaticAssignment e = ana build (mempty, mempty, e) where
  build :: (Set.Set Name, Map.Map Name Int, Exp) -> ExpF (Set.Set Name, Map.Map Name Int, Exp)
  build (vars, subst, e) = case e of
    EBind lhs v@(Var nm) rhs
      -- The name is already in scope, and it has a renaming yet.
      | and [Set.member nm vars, Map.member nm subst] ->
          let (Just n) = Map.lookup nm subst
              n' = n + 1
              nm' = newName nm n'
          in EBindF
              (Set.insert nm' vars, Map.insert nm n' subst, lhs)
              (Var nm')
              (Set.insert nm' vars, Map.insert nm n' subst, rhs)

      -- The name is already in scope, the but does not have a renaming yet.
      | Set.member nm vars ->
          let n' = 1
              nm' = newName nm n'
          in EBindF
              (Set.insert nm' vars, Map.insert nm 1 subst, lhs)
              (Var nm')
              (Set.insert nm' vars, Map.insert nm 1 subst, rhs)

      -- Remember the variable name.
      | otherwise -> EBindF (Set.insert nm vars, subst, lhs) v (Set.insert nm vars, subst, rhs)

    -- Substituitions
    ECase       val alts -> ECaseF (substVal val) (((,,) vars subst) <$> alts)
    SApp        name params -> SAppF name (substVal <$> params)
    SReturn     val -> SReturnF (substVal val)
    SStore      val -> SStoreF (substVal val)
    SFetchI     name pos -> SFetchIF (substName name) pos
    SUpdate     name val -> SUpdateF (substName name) (substVal val)

    rest -> ((,,) vars subst) <$> project rest
    where
      newName nm i = concat [nm, "_", show i]
      substVal = \case
        ConstTagNode  tag params -> ConstTagNode tag (substVal <$> params)
        VarTagNode    nm  params -> VarTagNode (substName nm) (substVal <$> params)
        Var nm  -> Var $ substName nm
        rest    -> rest

      substName nm = maybe nm (newName nm) (Map.lookup nm subst)
