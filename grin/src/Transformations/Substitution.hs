{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor, OverloadedStrings #-}
module Transformations.Substitution where

import Data.Map (Map)
import Free
import Grin
import Test.Hspec

import qualified Data.Map as Map
import Data.Functor.Foldable as Foldable



-- | Substitute a value for a variable.
substitution :: Map.Map Name Val -> Exp -> Exp
substitution substituitons = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    Program  defs               -> ProgramF defs
    Def      name names exp     -> DefF name names exp
    -- Exp
    EBind    simpleExp lpat exp -> EBindF simpleExp lpat exp
    ECase    val alts           -> ECaseF (subst val) alts
    -- Simple Exp
    SApp     name simpleVals    -> SAppF    name (subst <$> simpleVals)
    SReturn  val                -> SReturnF (subst val)
    SStore   val                -> SStoreF  (subst val)
    SFetchI  name index         -> SFetchIF  name index
    SUpdate  name val           -> SUpdateF name (subst val)
    SBlock   exp                -> SBlockF  exp
    -- Alt
    Alt pat exp                 -> AltF pat exp


  subst :: Val -> Val
  subst (Var name) = case Map.lookup name substituitons of
    Nothing  -> Var name
    Just val -> val
  subst (ConstTagNode tag simpleVals) = ConstTagNode tag (subst <$> simpleVals)
  subst (ValTag tag)                  = ValTag tag
  subst (VarTagNode name simpleVals)  = VarTagNode name (subst <$> simpleVals)
  subst other                         = other


tests :: Spec
tests = do
  it "simple expressions" $ do
    x <- buildExpM $
      "x"  <=: store "a"                                  $
      "y"  <=: app "intAdd" ["a", i64 3, "a", i64 2, "a"] $
      Unit <=: fetch "b" (Just 2)                         $
      Unit <=: fetch "b" Nothing                          $
      unit "y"
    e <- buildExpM $
      "x"  <=: store (i64 10)                                      $
      "y"  <=: app "intAdd" [i64 10, i64 3, i64 10, i64 2, i64 10] $
      Unit <=: fetch "b" (Just 2)                                  $
      Unit <=: fetch "b" Nothing                                   $
      unit "y"
    -- 'a' should be replaced, 'b' should be left out
    (substitution (Map.fromList [("a", i64 10), ("b", i64 20)]) x)
      `shouldBe` e

sRunTests :: IO ()
sRunTests = hspec Transformations.Substitution.tests
