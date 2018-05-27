{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Frontend.Lambda.CodeGen (codegenGrin) where

import Text.Printf
import Control.Monad
import Control.Monad.State

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Frontend.Lambda.Syntax
import Frontend.Lambda.PrimOps
import qualified Grin as G
import Transformations.Optimising.DeadProcedureElimination
import Transformations.GenerateEval

data Env
  = Env
  { _counter  :: Int
  , _arityMap :: Map Name Int
  }

type CG = State Env

uniq :: Name -> CG Name
uniq name = state (\env@Env{..} -> (printf "%s.%d" name _counter, env {_counter = succ _counter}))

arity :: Map Name Int -> Name -> Maybe Int
arity = flip Map.lookup

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
genC e = gets _arityMap >>= \arityMap -> case e of
  App name args | argCount <- length args
                , Just ar <- arity arityMap name -> case argCount `compare` ar of
    EQ  -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.F name) $ map genAtom args
    LT  -> pure $ G.SStore $ G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) $ map genAtom args
  -- TODO: use ap for suspended application
  App name [arg] | Nothing <- arity arityMap name -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.F "ap") [G.Var name, genAtom arg]
  --App name args | Nothing <- arity arityMap name -> -- ap store chain
  Con name args -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.C name) $ map genAtom args
  x -> error $ printf "unsupported C: %s" $ show x

-- strict context ; evaluates to WHNF
genE :: Exp -> CG G.Exp
genE e = gets _arityMap >>= \arityMap -> case e of
  App name args | argCount <- length args
                , Just ar <- arity arityMap name -> case argCount `compare` ar of
    EQ  -> pure $ G.SApp name $ map genAtom args
    LT  -> pure $ G.SReturn $ G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) $ map genAtom args
    GT  -> let (funArgs, extraArgs) = splitAt ar args
           in apChain (G.SApp name $ map genAtom funArgs) extraArgs
  -- HINT: unknown function ; generate apply chain
  App name args | argCount <- length args
                , Nothing <- arity arityMap name
        -- apply chain
        -> apChain (G.SApp "eval" [G.Var name]) args
  -- TODO: track if var is in WHNF already
  Var name -> pure $ G.SApp "eval" [G.Var name] -- TODO: handle functions
  x -> error $ printf "unsupported E: %s" $ show x

-- strict and return context (evaluates to WHNF) ; R is similar to E
genR :: Exp -> CG G.Exp
genR e = gets _arityMap >>= \arityMap -> case e of
  -- TODO: build var name <--> location name map for suspended computations
  Let  binds exp -> foldr (\(name, e) rightExp -> G.EBind <$> genC e <*> pure (G.Var name {-TODO-}) <*> rightExp) (genR exp) binds
  LetS binds exp -> foldr (\(name, e) rightExp -> G.EBind <$> genE e <*> pure (G.Var name) <*> rightExp) (genR exp) binds

  Con name args -> pure $ G.SReturn $ G.ConstTagNode (G.Tag G.C name) $ map genAtom args

  Case exp alts -> do
    whnf <- uniq "value"
    G.EBind <$> genE exp <*> pure (G.Var whnf) <*> (G.ECase (G.Var whnf) <$> mapM genR alts) -- TODO: handle name mapping for var name -> whnf value name
  Alt pat exp -> G.Alt (genCPat pat) <$> genR exp

  Program defs      -> G.Program <$> mapM genR defs
  Def name args exp -> G.Def name args <$> genR exp

  Lit lit -> pure $ G.SReturn $ G.Lit $ genLit lit
  App name args | argCount <- length args
                , Just ar <- arity arityMap name -> case argCount `compare` ar of
    EQ  -> pure $ G.SApp name $ map genAtom args
    LT  -> pure $ G.SReturn $ G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) $ map genAtom args
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
    - generate pointer names for suspended computations
    done - generate eval function
    done - generate apply function
    - letrec and circular data structures
    done - primop
    - fill R, E, C
    - rewrite as Grin anamorphism
    done - higher order sample
    - circular data sample
-}

codegenGrin :: Program -> G.Program
codegenGrin exp = generateEval . G.Program $ prog ++ primOps where
  G.Program prog    = evalState (genR exp) (Env 0 $ buildArityMap exp)
  G.Program primOps = lambdaPrimOps

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program defs) =
  Map.fromList $
    [(name, length args) | Def name args _ <- defs] ++
    [(name, length args) | let G.Program ops = lambdaPrimOps, G.Def name args _ <- ops]
buildArityMap _ = error "invalid expression, program expected"
