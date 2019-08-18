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
    it "Impossible alternative" $ do
      let before = [prog|
            grinMain =
              a0 <- pure 5
              n0 <- pure (CInt a0)
              r <- case n0 of
                (CInt  c0) -> pure 0
                (CBool c1) -> pure 0
              pure r
          |]

      let after = [prog|
            grinMain =
              a0 <- pure 5
              n0 <- pure (CInt.0)
              r <- case n0 of
                (CInt.0) ->
                  c0 <- pure (#undefined :: T_Int64)
                  pure 0
                (CBool.0) ->
                  c1 <- pure (#undefined :: T_Dead)
                  pure 0
              pure r
          |]
      pipelineSrc before after deadDataEliminationPipeline

    it "Length" $ pipeline
      "dead-data-elimination/length.grin"
      "dead-data-elimination/length.grin.expected"
      deadDataEliminationPipeline

    it "Multiple fields" $ do
      let before = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0
              n0 <- pure (CThree a0 a1 a2 a3 a4 a5)
              (CThree b0 b1 b2 b3 b4 b5) <- pure n0
              r <- _prim_int_add b1 b4
              pure r
          |]

      let after = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0
              n0 <- pure (CThree.0 a1 a4)
              (CThree.0 b1 b4) <- pure n0
              b5 <- pure (#undefined :: T_Int64)
              b3 <- pure (#undefined :: T_Int64)
              b2 <- pure (#undefined :: T_Int64)
              b0 <- pure (#undefined :: T_Int64)
              r <- _prim_int_add b1 b4
              pure r
          |]
      pipelineSrc before after deadDataEliminationPipeline

    it "Only dummify" $ do
      let before = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0

              -- two producers
              n0 <- pure (CThree a0 a1 a2)
              n1 <- pure (CThree a3 a4 a5)

              -- n01 has producers: n0, n1
              n01 <- case 0 of
                0 -> pure n0
                1 -> pure n1

              -- consumers
              (CThree b0 b1 b2) <- pure n0
              (CThree c0 c1 c2) <- pure n01
              (CThree d0 d1 d2) <- pure n1

              r <- pure (CLive b0 c1 d2)
              pure r
          |]

      let after = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0

              -- two producers
              n0 <- pure (CThree a0 a1 (#undefined :: T_Int64))
              n1 <- pure (CThree (#undefined :: T_Int64) a4 a5)

              -- n01 has producers: n0, n1
              n01 <- case 0 of
                0 -> pure n0
                1 -> pure n1

              -- consumers
              (CThree b0 b1 b2) <- pure n0
              (CThree c0 c1 c2) <- pure n01
              (CThree d0 d1 d2) <- pure n1

              r <- pure (CLive b0 c1 d2)
              pure r
          |]
      pipelineSrc before after deadDataEliminationPipeline

    it "Deletable Single" $ do
      let before = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0

              -- two producers
              n0 <- pure (CThree a0 a1 a2)
              n1 <- pure (CThree a3 a4 a5)

              -- n01 has producers: n0, n1
              n01 <- case 0 of
                0 -> pure n0
                1 -> pure n1

              -- consumers
              (CThree b0 b1 b2) <- pure n0
              (CThree c0 c1 c2) <- pure n01
              (CThree d0 d1 d2) <- pure n1

              r <- pure (CLive b0 d2)
              pure r
          |]

      let after = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0

              -- two producers
              n0 <- pure (CThree.0 a0 (#undefined :: T_Int64))
              n1 <- pure (CThree.0 (#undefined :: T_Int64) a5)

              -- n01 has producers: n0, n1
              n01 <- case 0 of
                0 -> pure n0
                1 -> pure n1

              -- consumers
              (CThree.0 b0 b2) <- pure n0
              b1 <- pure (#undefined :: T_Int64)
              (CThree.0 c0 c2) <- pure n01
              c1 <- pure (#undefined :: T_Int64)
              (CThree.0 d0 d2) <- pure n1
              d1 <- pure (#undefined :: T_Int64)

              r <- pure (CLive b0 d2)
              pure r
          |]
      pipelineSrc before after deadDataEliminationPipeline

    it "Deletable Multi" $ do
      let before = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0

              -- two producers
              n0 <- pure (CThree a0 a1 a2)
              n1 <- pure (CThree a3 a4 a5)

              -- n01 has producers: n0, n1
              n01 <- case 0 of
                0 -> pure n0
                1 -> pure n1

              -- consumers
              (CThree b0 b1 b2) <- pure n0
              (CThree c0 c1 c2) <- pure n01
              (CThree d0 d1 d2) <- pure n1

              r <- pure (CLive c1)
              pure r
          |]

      let after = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0
              a3 <- pure 0
              a4 <- pure 0
              a5 <- pure 0

              -- two producers
              n0 <- pure (CThree.0 a1)
              n1 <- pure (CThree.0 a4)

              -- n01 has producers: n0, n1
              n01 <- case 0 of
                0 -> pure n0
                1 -> pure n1

              -- consumers
              (CThree.0 b1) <- pure n0
              b2 <- pure (#undefined :: T_Int64)
              b0 <- pure (#undefined :: T_Int64)

              (CThree.0 c1) <- pure n01
              c2 <- pure (#undefined :: T_Int64)
              c0 <- pure (#undefined :: T_Int64)

              (CThree.0 d1) <- pure n1
              d2 <- pure (#undefined :: T_Int64)
              d0 <- pure (#undefined :: T_Int64)

              r <- pure (CLive c1)
              pure r
          |]
      pipelineSrc before after deadDataEliminationPipeline

    it "Separate Prods" $ do
      let before = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0

              n0 <- pure (CThree a0 a1 a2)
              n1 <- pure (CThree a0 a1 a2)
              n2 <- pure (CThree a0 a1 a2)
              n3 <- pure (CThree a0 a1 a2)
              n4 <- pure (CThree a0 a1 a2)
              n5 <- pure (CThree a0 a1 a2)
              n6 <- pure (CThree a0 a1 a2)
              n7 <- pure (CThree a0 a1 a2)

              -- consumers
              (CThree b0 b1 b2) <- pure n0
              (CThree c0 c1 c2) <- pure n1
              (CThree d0 d1 d2) <- pure n2
              (CThree e0 e1 e2) <- pure n3
              (CThree f0 f1 f2) <- pure n4
              (CThree g0 g1 g2) <- pure n5
              (CThree h0 h1 h2) <- pure n6
              (CThree i0 i1 i2) <- pure n7

              r <- pure (CLive b0 b1 b2 c0 d1 e2 f0 f1 g1 g2 h0 h2)
              pure r
          |]

      let after = [prog|
            grinMain =
              a0 <- pure 0
              a1 <- pure 0
              a2 <- pure 0

              n0 <- pure (CThree a0 a1 a2)
              n1 <- pure (CThree.6 a0)
              n2 <- pure (CThree.5 a1)
              n3 <- pure (CThree.4 a2)
              n4 <- pure (CThree.3 a0 a1)
              n5 <- pure (CThree.2 a1 a2)
              n6 <- pure (CThree.1 a0 a2)
              n7 <- pure (CThree.0)

              (CThree b0 b1 b2) <- pure n0

              (CThree.6 c0) <- pure n1
              c2 <- pure (#undefined :: T_Int64)
              c1 <- pure (#undefined :: T_Int64)

              (CThree.5 d1) <- pure n2
              d2 <- pure (#undefined :: T_Int64)
              d0 <- pure (#undefined :: T_Int64)

              (CThree.4 e2) <- pure n3
              e1 <- pure (#undefined :: T_Int64)
              e0 <- pure (#undefined :: T_Int64)

              (CThree.3 f0 f1) <- pure n4
              f2 <- pure (#undefined :: T_Int64)

              (CThree.2 g1 g2) <- pure n5
              g0 <- pure (#undefined :: T_Int64)

              (CThree.1 h0 h2) <- pure n6
              h1 <- pure (#undefined :: T_Int64)

              (CThree.0) <- pure n7
              i2 <- pure (#undefined :: T_Int64)
              i1 <- pure (#undefined :: T_Int64)
              i0 <- pure (#undefined :: T_Int64)

              r <- pure (CLive b0 b1 b2 c0 d1 e2 f0 f1 g1 g2 h0 h2)
              pure r
          |]
      pipelineSrc before after deadDataEliminationPipeline

    it "FNode Before" $ do
      let before = [prog|
            grinMain =
              x0 <- pure (CInt 5)
              p0 <- store x0
              a0 <- pure (Ffoo p0 p0 p0)
              p1 <- store a0
              a1 <- eval p1
              pure a1

            -- functions cannot return pointers
            foo x y z =
              y' <- eval y
              pure y'

            eval p =
              v <- fetch p
              case v of
                (CInt n) -> pure v
                (Ffoo x1 y1 z1) ->
                  w <- foo x1 y1 z1
                  update p w
                  pure w
          |]

      let after = [prog|
            grinMain =
              x0 <- pure (CInt 5)
              p0 <- store x0
              a0 <- pure (Ffoo.0 p0)
              p1 <- store a0
              a1 <- eval p1
              pure a1

            foo x y z =
              y' <- eval y
              pure y'

            eval p =
              v <- fetch p
              case v of
                (CInt n) -> pure v
                (Ffoo.0 y1) ->
                  z1 <- pure (#undefined :: #ptr)
                  x1 <- pure (#undefined :: #ptr)
                  w <- foo x1 y1 z1
                  update p w
                  pure w
          |]
      pipelineSrc before after deadDataEliminationPipeline

    it "PNode Before" $ pipeline
      "dead-data-elimination/pnode.grin"
      "dead-data-elimination/pnode.grin.expected"
      deadDataEliminationPipeline

    it "PNode Opt Before" $ do
      let before = [prog|
            grinMain =
              a0 <- pure (CInt 5)
              a1 <- pure (CInt 5)
              a2 <- pure (CInt 5)
              p0 <- store a0
              p1 <- store a1
              p2 <- store a2

              foo3   <- pure (P3foo)

              (P3foo) <- pure foo3
              foo2    <- pure (P2foo p0)

              (P2foo v0) <- pure foo2
              foo1       <- pure (P1foo v0 p1)

              (P1foo v1 v2) <- pure foo1
              fooRet        <- foo v1 v2 p2

              pure fooRet


            foo x0 y0 z0 =
              y0' <- fetch y0
              (CInt n) <- y0'
              pure y0'
          |]

      let after = [prog|
            {- NOTE:
              P2foo is renamed to P2foo.1 because the name generation
              takes the node name as base. So here foo will be the base,
              for which a name was already generated: P1foo.0.
            -}
            grinMain =
              a0 <- pure (CInt.0)
              a1 <- pure (CInt 5)
              a2 <- pure (CInt.0)
              p0 <- store a0
              p1 <- store a1
              p2 <- store a2

              foo3   <- pure (P3foo)

              (P3foo) <- pure foo3
              foo2    <- pure (P2foo.1)

              (P2foo.1) <- pure foo2
              v0        <- pure (#undefined :: #ptr)
              foo1      <- pure (P1foo.0 p1)

              (P1foo.0 v2) <- pure foo1
              v1           <- pure (#undefined :: #ptr)
              fooRet       <- foo v1 v2 p2

              pure fooRet


            foo x0 y0 z0 =
              y0' <- fetch y0
              (CInt n) <- y0'
              pure y0'
          |]
      pipelineSrc before after deadDataEliminationPipeline

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
  | (lvaProgram, lvaMapping) <- LiveVariable.codeGen prog
  , computer <- _airComp . evalAbstractProgram $ lvaProgram
  = toLVAResult lvaMapping computer

calcCByResult :: Exp -> CByResult
calcCByResult prog
  | (cbyProgram, cbyMapping) <- CreatedBy.codeGen prog
  , computer <- _airComp . evalAbstractProgram $ cbyProgram
  , cbyResult <- toCByResult cbyMapping computer
  = cbyResult
