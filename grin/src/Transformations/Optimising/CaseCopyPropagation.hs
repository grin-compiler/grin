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

data Changes = None | InChange | Final deriving (Eq, Show)

data NewExp = NE
  { neChanges :: Changes
  , neExp     :: Exp
  , neTag     :: Maybe Tag
  } deriving Show

folder :: ExpF (Exp, NewExp) -> NameM NewExp
folder = \case
  ProgramF ds                         -> pure $ NE Final (Program $ map (neExp . snd) ds) Nothing
  DefF n ps body@(b, NE InChange _ _) -> pure $ NE Final (Def n ps b) Nothing
  DefF n ps body@(_, NE c e t)        -> pure $ NE c (Def n ps e) t

  SReturnF (ConstTagNode tag [value]) -> pure $ NE InChange (SReturn value) (Just tag)

  SBlockF (b, NE InChange _ _) -> pure $ NE None (SBlock b) Nothing
  SBlockF (_, NE c e Nothing)  -> pure $ NE c    (SBlock e) Nothing
  exp@SBlockF{}                -> error $ "Invalid Block:" ++ show exp

  exp | isPrimitiveExp (embed $ fmap fst exp) -> pure $ NE None (embed $ fmap (neExp . snd) exp) Nothing

  EBindF (_, NE None  lhse Nothing) pat (_, NE None rhse Nothing)           -> pure $ NE None (EBind lhse pat rhse) Nothing
  EBindF (_, NE None  lhse Nothing) pat (_, NE InChange rhse rtag@(Just _)) -> pure $ NE InChange (EBind lhse pat rhse) rtag
  EBindF (_, NE None  lhse Nothing) pat (_, NE Final rhse Nothing)          -> pure $ NE Final (EBind lhse pat rhse) Nothing
  EBindF (_, NE Final lhse Nothing) pat (_, NE None  rhse Nothing)          -> pure $ NE Final (EBind lhse pat rhse) Nothing
  EBindF (_, NE Final lhse Nothing) pat (rhso, NE InChange _ (Just _))      -> pure $ NE Final (EBind lhse pat rhso) Nothing
  EBindF (_, NE Final lhse Nothing) pat (_, NE Final rhse Nothing)          -> pure $ NE Final (EBind lhse pat rhse) Nothing
  EBindF (lhso, _) pat (_, NE change rhse tag) | isPrimitiveExp lhso        -> pure $ NE change (EBind lhso pat rhse) tag
  exp@EBindF{} -> error $ "Invalid Bind:" ++ show exp

  AltF cpat body@(_, NE c e t) -> pure $ NE c (Alt cpat e) t

  exp@(ECaseF val alts)
    | all ((&&) <$> hasChanges InChange <*> (isJust . neTag . snd)) alts
      -> if allSame $ map (neTag . snd) alts
          then do var <- deriveNewName "ccp"
                  let (Just tag) = neTag $ snd $ head alts
                  pure $ NE
                    Final
                    (SBlock (EBind
                        (ECase val (map (neExp . snd) alts))
                        (Var var)
                        (SReturn (ConstTagNode tag [Var var]))))
                    Nothing
          else pure $ NE None (ECase val (map fst alts)) Nothing
    | any (hasChanges Final) alts
        -> pure $ NE Final (ECase val $ map getFinalExp alts) Nothing
    | all (hasChanges None) alts
        -> pure $ NE None (ECase val $ map (neExp . snd) alts) Nothing
    | otherwise -> error $ "Invalid ECase: " ++ show exp

  where
    hasChanges c0 (_, NE c _ _) = c == c0
    getFinalExp (oe, NE c ne _) = case c of { Final -> ne; _ -> oe }

allSame :: (Eq a) => [a] -> Bool
allSame [] = False
allSame (x:xs) = all (x ==) xs
