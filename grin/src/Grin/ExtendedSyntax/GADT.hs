{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Grin.ExtendedSyntax.GADT
  ( Role(..)
  , Exp(..)
  , convertToExp
  ) where

import Data.Kind (Constraint)
import GHC.TypeLits
import Grin.ExtendedSyntax.Syntax hiding (Exp(..))

import qualified Grin.ExtendedSyntax.Syntax as E (Exp(..))


-- | Through the role of an expression, structurally correct
-- expressions can be created.
data Role
  = Simple'   -- Simple Expressions
  | Bind'     -- Expressions
  | Case'     -- Case Expression
  | Alt'      -- Alternative of a case
  | Def'      -- Function definitions
  | Prg'      -- Program definition

-- | Expression that can represent a GRIN program.
data Exp (role' :: Role) where

  Program
    :: [External]   -- ^ Primitive operations and externals
    -> [Exp 'Def']  -- ^ Function definitions
    -> Exp 'Prg'

  Def
    :: Name         -- ^ Function name
    -> [Name]       -- ^ Function arguments name
    -> Exp 'Bind'   -- ^ Function body
    -> Exp 'Def'

  App
    :: Name         -- ^ Function applications
    -> [Name]       -- ^ Arguments
    -> Exp 'Simple'

  Pure
    :: Val          -- ^ Value should be a variable name, a Node, or a Literal
    -> Exp 'Simple'

  Store
    :: Name         -- ^ Variable that should have a location value
    -> Exp 'Simple'

  Fetch
    :: Name         -- ^ Variable that should have a location value
    -> Exp 'Simple'

  Update
    :: Name         -- ^ Variable that should have a location value
    -> Name         -- ^ Variable that should contain a node value
    -> Exp 'Simple'

  Alt
    :: CPat         -- ^ Pattern to match against
    -> Name         -- ^ Name of the alternative
    -> Exp 'Bind'   -- ^ Body of the alternative
    -> Exp 'Alt'

  Case
    :: Name         -- ^ Scrutinee variable
    -> [Exp 'Alt']  -- ^ Possible case alternatives
    -> Exp 'Case'

  -- |
  -- > Bind lhs bpat rhs
  -- corresponds to
  -- > lhs >>= \bpat -> rhs
  Bind
    :: (HasRole lhs ['Simple', 'Case'], HasRole rhs ['Simple', 'Case', 'Bind'])
    => Exp lhs      -- ^ First expression of the bind, it should be a Simple or a Case expression
    -> BPat         -- ^ Pattern to match against
    -> Exp rhs      -- ^ Second expression of the bind, it should be a Simple, Case or a Bind expression
    -> Exp 'Bind'


type HasRole c cs = Elem c cs cs

type family Elem (c :: Role) (xs :: [Role]) (cs :: [Role]) :: Constraint where
  Elem r rs (r : _)  = ()
  Elem r rs (d : cs) = Elem r rs cs
  Elem r rs '[]      = TypeError ('Text "Expected expression should have one role of " ':<>: 'ShowType rs
                                  ':$$:
                                  'Text "but got " ':<>: 'ShowType r)


convertToExp :: forall r . Exp r -> E.Exp
convertToExp = \case
  Program exts defs   -> E.Program exts (convertToExp <$> defs)
  Def name args body  -> E.Def name args (convertToExp body)
  App name args       -> E.SApp name args
  Pure val            -> E.SReturn val
  Store name          -> E.SStore name
  Fetch name          -> E.SFetch name
  Update lName nName  -> E.SUpdate lName nName
  Case name alts      -> E.ECase name (convertToExp <$> alts)
  Alt cpat name body  -> E.Alt cpat name (convertToExp body)
  Bind lhs bpat rhs   -> E.EBind (convertToExp lhs) bpat (convertToExp rhs)
