module GhcDump.Util
    ( -- * Convenient IO
      readDump, readDump'
      -- * Manipulating 'Type's
    , splitFunTys
    , splitForAlls
      -- * Manipulating expressions
    , collectArgs
    , collectBinders
    , collectTyBinders
    ) where

import Prelude hiding (readFile)

import qualified Data.ByteString.Lazy as BSL
import Data.Binary

import GhcDump_Ast
import GhcDump.Reconstruct

readDump' :: FilePath -> IO SModule
readDump' fname = decode <$> BSL.readFile fname

readDump :: FilePath -> IO Module
readDump fname = reconModule <$> readDump' fname

splitFunTys :: Type' bndr var -> [Type' bndr var]
splitFunTys = go []
  where
    go acc (FunTy a b) = go (a : acc) b
    go acc t = reverse (t : acc)

splitForAlls :: Type' bndr var -> ([bndr], Type' bndr var)
splitForAlls = go []
  where
    go acc (ForAllTy b t) = go (b : acc) t
    go acc t              = (reverse acc, t)

collectBinders :: Expr' bndr var -> ([bndr], Expr' bndr var)
collectBinders = go []
  where
    go :: [bndr] -> Expr' bndr var -> ([bndr], Expr' bndr var)
    go acc (ELam v x) = go (v : acc) x
    go acc x          = (reverse acc, x)

collectTyBinders :: Expr' bndr var -> ([bndr], Expr' bndr var)
collectTyBinders = go []
  where
    go :: [bndr] -> Expr' bndr var -> ([bndr], Expr' bndr var)
    go acc (ETyLam v x) = go (v : acc) x
    go acc x            = (reverse acc, x)

collectArgs :: Expr' bndr var -> (Expr' bndr var, [Expr' bndr var])
collectArgs = go []
  where
    go :: [Expr' bndr var] -> Expr' bndr var -> (Expr' bndr var, [Expr' bndr var])
    go acc (EApp x y) = go (y : acc) x
    go acc x          = (x, acc)
