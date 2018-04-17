{-# LANGUAGE LambdaCase, TupleSections #-}
module Frontend.CodeGen (codegenGrin) where

import Text.Printf
import Control.Monad
import Control.Monad.State

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.Lambda
import qualified Grin as G

type CG = State (Map Name Int) -- TODO

uniq :: Name -> CG Name
uniq name = pure name -- TODO

arity :: Map Name Int -> Name -> Maybe Int
arity = flip Map.lookup -- TODO

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
  x -> error $ printf "unsupported atom: %s" $ show x

apChain :: G.Exp -> [Atom] -> CG G.Exp
apChain exp args = foldr ap (pure exp) args where -- TODO: fix the order
  ap arg leftExp = do
    argWhnf <- uniq "arg"
    G.EBind <$> leftExp <*> pure (G.Var argWhnf) <*> pure (G.SApp "apply" [G.Var argWhnf, genAtom arg])

-- lazy context ; constructs suspended computations
genC :: Exp -> CG G.Exp
genC e = get >>= \arityMap -> case e of
  App name args | argCount <- length args
                , Just ar <- arity arityMap name -> case argCount `compare` ar of
    EQ  -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.F name) $ map genAtom args
    LT  -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.P $ name ++ show (ar - argCount)) $ map genAtom args
  x -> error $ printf "unsupported C: %s" $ show x

-- strict context ; evaluates to WHNF
genE :: Exp -> CG G.Exp
genE e = get >>= \arityMap -> case e of
  App name args | argCount <- length args
                , Just ar <- arity arityMap name -> case argCount `compare` ar of
    EQ  -> pure $ G.SApp name $ map genAtom args
    LT  -> pure $ G.SReturn $ G.ConstTagNode (G.Tag G.P $ name ++ show (ar - argCount)) $ map genAtom args
    GT  -> let (funArgs, extraArgs) = splitAt ar args
           in apChain (G.SApp name $ map genAtom funArgs) extraArgs
  -- HINT: unknown function ; generate apply chain
  App name args | argCount <- length args
                , Nothing <- arity arityMap name
        -- apply chain
        -> apChain (G.SApp "eval" [G.Var name]) args
  Var name -> pure $ G.SApp "eval" [G.Var name] -- TODO: handle functions
  x -> error $ printf "unsupported E: %s" $ show x

-- strict and return context (evaluates to WHNF) ; R is similar to E
genR :: Exp -> CG G.Exp
genR e = get >>= \arityMap -> case e of
  -- TODO: build var name <--> location name map for suspended computations
  Let  binds exp -> foldr (\(name, e) rightExp -> G.EBind <$> genC e <*> pure (G.Var name {-TODO-}) <*> rightExp) (genR exp) binds
  LetS binds exp -> foldr (\(name, e) rightExp -> G.EBind <$> genE e <*> pure (G.Var name) <*> rightExp) (genR exp) binds

  Con name args -> pure $ G.SReturn $ G.ConstTagNode (G.Tag G.C name) $ map genAtom args

  Case exp alts -> G.EBind <$> genE exp <*> pure (genAtom exp) <*> (G.ECase (genAtom exp) <$> mapM genR alts)
  Alt pat exp -> G.Alt (genCPat pat) <$> genR exp

  Program defs      -> G.Program <$> mapM genR defs
  Def name args exp -> G.Def name args <$> genR exp

  Lit lit -> pure $ G.SReturn $ G.Lit $ genLit lit
  App name args | argCount <- length args
                , Just ar <- arity arityMap name -> case argCount `compare` ar of
    EQ  -> pure $ G.SApp name $ map genAtom args
    LT  -> pure $ G.SReturn $ G.ConstTagNode (G.Tag G.P $ name ++ show (ar - argCount)) $ map genAtom args
    GT  -> let (funArgs, extraArgs) = splitAt ar args
           in apChain (G.SApp name $ map genAtom funArgs) extraArgs
  -- HINT: unknown function ; generate apply chain
  App name args | argCount <- length args
                , Nothing <- arity arityMap name
        -- apply chain
        -> apChain (G.SApp "eval" [G.Var name]) args

  x -> error $ printf "unsupported R: %s" $ show x

{-
  TODO:
    done - unknown function: apply chain
    done - over application
    - generate eval function
    - generate apply function
    - letrec and circular data structures
    - primop
    - fill R, E, C
    - rewrite as Grin anamorphism
    - higher order sample
    - circular data sample
-}

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

codegenGrin :: Program -> G.Program
codegenGrin exp = evalState (genR exp) (buildArityMap exp)

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
