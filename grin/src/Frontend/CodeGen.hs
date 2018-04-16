{-# LANGUAGE LambdaCase, TupleSections #-}
module Frontend.CodeGen (codegenGrin) where

import Control.Monad
import Control.Monad.State

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.Lambda
import qualified Grin as G

type CG = State ()

arity :: Name -> Int
arity = undefined

genLit :: Lit -> G.Lit
genLit = \case
  LInt64  v -> G.LInt64 v
  LWord64 v -> G.LWord64 v
  LFloat  v -> G.LFloat v
  LBool   v -> G.LBool v

genCPat :: Pat -> G.CPat
genCPat = \case
  NodePat name args -> G.NodePat (G.Tag G.C name) args
  LitPat  lit       -> G.LitPat (genLit lit)
  DefaultPat        -> G.DefaultPat

genAtom :: Atom -> G.Val
genAtom = \case
  Var name  -> G.Var name
  Lit lit   -> G.Lit $ genLit lit
  x -> error $ show x

-- lazy context ; constructs suspended computations
genC :: Exp -> CG G.Exp
genC = \case
  App name args | argCount <- length args
                , ar <- arity name -> case argCount `compare` ar of
    EQ  -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.F name) $ map genAtom args
    LT  -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.F $ name ++ show (ar - argCount)) $ map genAtom args
  x -> error $ show x

-- strict context ; evaluates to WHNF
genE :: Exp -> CG G.Exp
genE = \case
  App name args | argCount <- length args
                , ar <- arity name -> case argCount `compare` ar of
    EQ  -> pure $ G.SApp name $ map genAtom args
    LT  -> pure $ G.SReturn $ G.ConstTagNode (G.Tag G.F $ name ++ show (ar - argCount)) $ map genAtom args
  Var name -> pure $ G.SApp "eval" [G.Var name]
  x -> error $ show x

-- strict and return context (evaluates to WHNF) ; R is similar to E
genR :: Exp -> CG G.Exp
genR = \case
  -- TODO: build var name <--> location name map for suspended computations
  Let  binds exp -> foldr (\(name, e) rightExp -> G.EBind <$> genC e <*> pure (G.Var name {-TODO-}) <*> rightExp) (genR exp) binds
  LetS binds exp -> foldr (\(name, e) rightExp -> G.EBind <$> genE e <*> pure (G.Var name) <*> rightExp) (genR exp) binds

  Con name args -> pure $ G.SReturn $ G.ConstTagNode (G.Tag G.C name) $ map genAtom args

  Case exp alts -> G.EBind <$> genE exp <*> pure (genAtom exp) <*> (G.ECase (genAtom exp) <$> mapM genR alts)
  Alt pat exp -> G.Alt (genCPat pat) <$> genR exp

  x -> error $ show x


{-
  App   C E
  Case  R
  Let   R
  LetS  R
  Con   R
  Var   E
  Lit
  Alt   R
-}

codegenGrin :: Exp -> G.Exp
codegenGrin _ = G.Program [] -- TODO

{-
  = Program     [Def]
  -- Binding
  | Def         Name [Name] Exp
  -- Exp
  | App         Name [Atom]
  | Case        Atom [Alt]
  | Let         [(Name, Exp)] Exp -- lazy let
  | LetS        [(Name, Exp)] Exp -- strict let
  | Con         Tag [Atom]
  -- Atom
  | Var         Name
  | Lit         Lit
  -- Alt
  | Alt         Pat Exp
-}

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program defs) = Map.fromList [(name, length args) | Def name args _ <- defs]
buildArityMap _ = error "invalid expression, program expected"
