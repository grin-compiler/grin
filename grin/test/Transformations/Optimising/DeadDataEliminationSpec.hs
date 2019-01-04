{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.Optimising.DeadDataEliminationSpec where

import Test.Hspec

import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin
import Grin.TH
import Grin.TypeCheck (typeEnvFromHPTResult)
import Test.Hspec.PipelineExample
import Pipeline.Pipeline hiding (pipeline)
import Transformations.Optimising.DeadDataElimination
import Transformations.Names (ExpChanges(..))
import Test.Util
import AbstractInterpretation.LiveVariable.CodeGen as LiveVariable (codeGen)
import AbstractInterpretation.LiveVariable.Result (LVAResult(..), toLVAResult)
import AbstractInterpretation.Reduce (evalAbstractProgram, _airComp)
import qualified AbstractInterpretation.CreatedBy.CodeGen as CreatedBy
import AbstractInterpretation.CreatedBy.Result
import AbstractInterpretation.CreatedBy.Readback (toCByResult)
import AbstractInterpretation.CreatedBy.Util


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  let deadDataEliminationPipeline =
        [ CBy Compile
        , CBy RunPure
        , LVA Compile
        , LVA RunPure
        , T DeadDataElimination
        ]

  describe "Dead Data Elimination" $ do
    it "Impossible alternative" $ pipeline
      "dead_data/before/impossible_alt.grin"
      "dead_data/after/impossible_alt.grin"
      deadDataEliminationPipeline

    it "Length" $ pipeline
      "dead_data/before/length.grin"
      "dead_data/after/length.grin"
      deadDataEliminationPipeline

    it "Multiple fields" $ pipeline
      "dead_data/before/multiple_fields.grin"
      "dead_data/after/multiple_fields.grin"
      deadDataEliminationPipeline

    it "Only dummify" $ pipeline
      "dead_data/before/only_dummify.grin"
      "dead_data/after/only_dummify.grin"
      deadDataEliminationPipeline

    it "Deletable Single" $ pipeline
      "dead_data/before/deletable_single.grin"
      "dead_data/after/deletable_single.grin"
      deadDataEliminationPipeline

    it "Deletable Multi" $ pipeline
      "dead_data/before/deletable_multi.grin"
      "dead_data/after/deletable_multi.grin"
      deadDataEliminationPipeline

    it "Separate Prods" $ pipeline
      "dead_data/before/separate_prods.grin"
      "dead_data/after/separate_prods.grin"
      deadDataEliminationPipeline

    it "FNode Before" $ pipeline
      "dead_code/fnode.grin"
      "dead_data/after/fnode.grin"
      deadDataEliminationPipeline

    it "PNode Before" $ pipeline
      "dead_code/pnode.grin"
      "dead_data/after/pnode.grin"
      deadDataEliminationPipeline

    it "PNode Opt Before" $ pipeline
      "dead_code/pnode_opt.grin"
      "dead_data/after/pnode_opt.grin"
      deadDataEliminationPipeline

  describe "Producer Grouping" $ do
    let exp = [prog|
          grinMain =
            n0 <- pure (CInt  0)
            n1 <- pure (CBool 0)
            n2 <- pure (CBool 0)
            n3 <- pure (CBool 0)
            s <- pure 5
            n01 <- case s of
              0 -> pure n0
              1 -> pure n1
            n12 <- case s of
              0 -> pure n1
              1 -> pure n2
            n23 <- case s of
              0 -> pure n2
              1 -> pure n3
            z0 <- case n01 of
              (CInt  c0) -> pure 5
              (CBool c1) -> pure 5
            (CBool z1) <- case n12 of
              (CInt  c2) -> pure 5
              (CBool c3) -> pure 5
            (CBool z2) <- pure n23
            pure 5
        |]

    it "multi_prod_simple_all" $ do
      let multiProdSimpleAllExpected = mkGraph
            [ ("n0", [ (cInt, ["n0"]) ] )
            , ("n1", [ (cBool, ["n1", "n2", "n3"]) ])
            , ("n2", [ (cBool, ["n1", "n2", "n3"]) ])
            , ("n3", [ (cBool, ["n1", "n2", "n3"]) ])
            ]
          found = groupAllProducers . _producers . calcCByResult $ exp
      found `shouldBe` multiProdSimpleAllExpected

    it "multi_prod_simple_active" $ do
      let multiProdSimpleActiveExpected = mkGraph
            [ ("n0", [ (cInt, ["n0"]) ])
            , ("n1", [ (cBool, ["n1"]) ])
            , ("n2", [ (cBool, ["n2"]) ])
            , ("n3", [ (cBool, ["n3"]) ])
            ]
      let found = groupActiveProducers <$> calcLiveness <*> (_producers . calcCByResult) $ exp
      found `shouldBe` multiProdSimpleActiveExpected

mkGraph :: [ (Name, [(Tag, [Name])]) ] -> ProducerGraph
mkGraph = toProducerGraph
        . Map.map (Map.map Set.fromList)
        . Map.map Map.fromList
        . Map.fromList

calcLiveness :: Exp -> LVAResult
calcLiveness prog
  | Right lvaProgram <- LiveVariable.codeGen prog
  , computer <- _airComp . evalAbstractProgram . fst $ lvaProgram
  = toLVAResult lvaProgram computer

calcCByResult :: Exp -> CByResult
calcCByResult prog
  | Right cbyProgram <- CreatedBy.codeGen prog
  , computer <- _airComp . evalAbstractProgram . fst $ cbyProgram
  , cbyResult <- toCByResult cbyProgram computer
  = cbyResult
