{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
module Transformations.Simplifying.RegisterIntroduction where

import Control.Arrow ((***), second)
import Data.Function
import Data.Map (Map)
import Data.Maybe
import Data.List (intercalate, foldl')
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Data.Functor.Infix
import Control.Monad.State
import Test.Hspec

import Grin.Grin

nth :: Int -> Int -> [a] -> [a]
nth s n = go 1 . drop s where
  go 1 (x:xs) = x:go n   xs
  go n (_:xs) = go (n-1) xs

nthSpec :: Spec
nthSpec = describe "nth" $ do
  it "works for 0 2" $ do
    (take 5 $ nth 0 2 [1..]) `shouldBe` [1,3,5,7,9]
  it "works for 1 2" $ do
    (take 5 $ nth 1 2 [1..]) `shouldBe` [2,4,6,8,10]

registerIntroductionI :: Int -> Exp -> Exp
registerIntroductionI _ e = apo builder ([1..], e) where
  builder :: ([Int], Exp) -> ExpF (Either Exp ([Int], Exp))
  builder (path, exp) =
    case exp of
      SStore (VarTagNode name vals)         -> varTagNode   SStore          name vals
      SStore (ConstTagNode tag vals)        -> constTagNode SStore          tag vals
      SStore (Lit lit)                      -> literal      SStore          lit
      SReturn (VarTagNode name vals)        -> varTagNode   SReturn         name vals
      SReturn (ConstTagNode tag vals)       -> constTagNode SReturn         tag vals
      SUpdate uname (VarTagNode tname vals) -> varTagNode   (SUpdate uname) tname vals
      SUpdate uname (ConstTagNode tag vals) -> constTagNode (SUpdate uname) tag vals
      SUpdate uname (Lit lit)               -> literal      (SUpdate uname) lit
      SApp name vals                        -> appExp (if any isLit vals then SBlock else id) name vals

      Program exts defs ->
        let n = length defs
        in ProgramF exts $ zipWith (\i d -> Right (nth i n path', d)) [1..] defs
      EBind sexp lpat exp                   -> EBindF (Right (nth 0 2 path', sexp)) lpat (Right (nth 1 2 path', exp))
      ECase val alts                        -> let n = length alts
                                               in ECaseF val $ zipWith (\i a -> Right (nth i n path', a)) [0..] alts

      e -> fmap (\e' -> Right (path', e')) $ project e -- (Right . (,) (tail path)) $ project e

    where
      path' = tail path
      evars = map (\i -> packName $ concat ["v.", show (head path), ".", show i]) [1..]

      changeSimpleVals :: [Name] -> [SimpleVal] -> ([SimpleVal], [(Name, Val)])
      changeSimpleVals newVars svals = second catMaybes . unzip $ zipWith changeVal svals newVars
        where
          changeVal (Lit lit)  v = (Var v, Just (v, Lit lit))
          changeVal (Var v)    _ = (Var v, Nothing)
          changeVal (ValTag g) v = (Var v, Just (v, ValTag g)) -- constTagNode only
          changeVal bad        _ = error $ unwords ["registerIntroduction changeSimpleVals: invalid simple literal:", show bad]

      literal context lit =
        fmap Left . project $ EBind (SReturn (Lit lit)) (Var (evars !! 0)) (context (Var $ evars !! 0))

      introduction block context vals =
        let (vals', newVars) = changeSimpleVals evars vals
        in fmap Left . project . block $ foldr
            (\(name, lit) -> EBind (SReturn lit) (Var name))
            (context (fst <$> listToMaybe newVars) vals') -- Tag is always first and stand for constTagNode only
            newVars

      appExp       block name = introduction block (const $ SApp name)
      varTagNode   context name = introduction id (const $ context . VarTagNode name)
      constTagNode context tag vals =
        introduction SBlock
          (\(Just t) vs -> context $ VarTagNode t (tail vs))
          ((ValTag tag):vals)

tests :: Spec
tests = do
  nthSpec
