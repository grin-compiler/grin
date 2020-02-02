module Reducer.ExtendedSyntax.BaseSpec where

import Reducer.ExtendedSyntax.Base

import Data.Text
import qualified Data.Map as Map

import Test.Hspec

import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Syntax
import Grin.ExtendedSyntax.TypeEnvDefs
import Grin.ExtendedSyntax.Pretty (KeyValueMap(..))


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Basic reducer" $ do
    it "interprets literals correctly" $ do
      let lit = LBool True

      let result   = evalVal mempty (Lit lit)
          expected = RT_Lit lit

      result `sameAs` expected

    it "interprets variables correctly" $ do
      let varName = "v"
          rtVal   = RT_Lit $ LInt64 42
          initEnv = Map.singleton varName rtVal

      let result   = evalVal initEnv (Var varName)
          expected = rtVal

      result `sameAs` expected

    it "interprets nodes correctly" $ do
      let varName = "v"
          rtVal   = RT_Lit $ LString "asd"
          tag     = Tag C "Int"
          initEnv = Map.singleton varName rtVal

      let result   = evalVal initEnv (ConstTagNode tag [varName])
          expected = RT_ConstTagNode tag [rtVal]

      result `sameAs` expected

    it "interprets unit correctly" $ do
      let result   = evalVal mempty Unit
          expected = RT_Unit

      result `sameAs` expected

    it "interprets undefined correctly" $ do
      let result   = evalVal mempty (Undefined $ T_SimpleType T_Dead)
          expected = RT_Undefined

      result `sameAs` expected

    it "interprets variables patterns correctly" $ do
      let varName = "v"
          rtVal   = RT_Lit $ LChar 'c'
          initEnv = Map.singleton "w" RT_Unit

      let result   = KV $ bindPat initEnv rtVal (VarPat varName)
          expected = KV $ Map.insert varName rtVal initEnv

      result `sameAs` expected

    it "interprets as-patterns correctly" $ do
      let varName   = "v"
          argName   = "a"
          tag       = Tag C "Float"
          argRtVal  = RT_Lit $ LFloat 42
          nodeRtVal = RT_ConstTagNode tag [argRtVal]
          initEnv   = Map.singleton "w" RT_Unit

      let result   = KV $ bindPat initEnv nodeRtVal (AsPat tag [argName] varName)
          expected = KV $ initEnv <> Map.fromList [(varName, nodeRtVal), (argName, argRtVal)]

      result `sameAs` expected
