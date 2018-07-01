{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.CaseCopyPropagation where

import Grin.Grin
import Grin.TypeEnv
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map ( fromList, union )
import Control.Monad.Trans.State
import Control.Monad.Trans ( lift )

import Data.Functor.Foldable
import Transformations.Names
import Transformations.Util



caseCopyPropagation :: Exp -> Exp
caseCopyPropagation exp0 = neExp $ evalNameM exp0 $ paraM folder exp0

data Changes = None | InChange Tag | Final deriving (Eq, Show)

data NewExp = NE
  { neChanges :: Changes
  , neExp     :: Exp
  } deriving Show

neTag :: NewExp -> Maybe Tag
neTag (NE (InChange t)_ ) = Just t
neTag _                   = Nothing

folder :: ExpF (Exp, NewExp) -> NameM NewExp
folder = \case
  ProgramF ds                           -> pure $ NE Final (Program $ map (neExp . snd) ds)
  DefF n ps body@(b, NE (InChange _) _) -> pure $ NE Final (Def n ps b)
  DefF n ps body@(_, NE c e)            -> pure $ NE c (Def n ps e)

  SReturnF (ConstTagNode tag [value]) -> pure $ NE (InChange tag) (SReturn value)

  SBlockF (b, NE (InChange _) _) -> pure $ NE None (SBlock b)
  SBlockF (_, NE c e)            -> pure $ NE c    (SBlock e)

  exp | isPrimitiveExp (embed $ fmap fst exp) -> pure $ NE None (embed $ fmap (neExp . snd) exp)

  EBindF (_, NE None  lhse) pat (_, NE None rhse)                -> pure $ NE None (EBind lhse pat rhse)
  EBindF (_, NE None  lhse) pat (_, NE (InChange t) rhse)        -> pure $ NE (InChange t) (EBind lhse pat rhse)
  EBindF (_, NE None  lhse) pat (_, NE Final rhse)               -> pure $ NE Final (EBind lhse pat rhse)
  EBindF (_, NE Final lhse) pat (_, NE None  rhse)               -> pure $ NE Final (EBind lhse pat rhse)
  EBindF (_, NE Final lhse) pat (rhso, NE (InChange _) _)        -> pure $ NE Final (EBind lhse pat rhso)
  EBindF (_, NE Final lhse) pat (_, NE Final rhse)               -> pure $ NE Final (EBind lhse pat rhse)
  EBindF (lhso, _) pat (_, NE change rhse) | isPrimitiveExp lhso -> pure $ NE change (EBind lhso pat rhse)

  AltF cpat body@(_, NE c e) -> pure $ NE c (Alt cpat e)

  exp@(ECaseF val alts)
    | tags <- map (neTag . snd) alts, all isJust tags
      -> if allSame tags
          then do var <- deriveNewName "ccp"
                  let (Just tag) = neTag $ snd $ head alts
                  pure $ NE
                    Final
                    (SBlock (EBind
                        (ECase val (map (neExp . snd) alts))
                        (Var var)
                        (SReturn (ConstTagNode tag [Var var]))))
          else pure $ NE None (ECase val (map fst alts))
    | any (hasChanges Final) alts
        -> pure $ NE Final (ECase val $ map getFinalExp alts)
    | all (hasChanges None) alts
        -> pure $ NE None (ECase val $ map (neExp . snd) alts)

  where
    hasChanges c0 (_, NE c _) = c == c0
    getFinalExp (oe, NE c ne) = case c of { Final -> ne; _ -> oe }

allSame :: (Eq a) => [a] -> Bool
allSame [] = False
allSame (x:xs) = all (x ==) xs
