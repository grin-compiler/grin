{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.Optimising.DeadVariableEliminationSpec where

import Test.Hspec
import Test.Hspec.PipelineExample
import Pipeline.Pipeline hiding (pipeline)
import qualified Pipeline.Pipeline as P
import Grin.TH
import Test.Assertions

-- TODO: Also refactor the last expressions based on the convention (they should of the form: `pure <var>`)

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Dead Variable Elimination" $ do

    let deadVariableEliminationPipeline =
          [ T DeadVariableElimination
          ]


    it "simple" $ do
      let before = [prog|
            grinMain =
              x0 <- pure 0
              x1 <- pure (CInt 0)
              x2 <- pure x0
              x3 <- pure x2
              pure 0
          |]

      let after = [prog|
            grinMain =
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "heap" $ do
      let before = [prog|
            grinMain =
              p0 <- store (CInt 0)
              n0 <- fetch p0
              p1 <- store n0
              y0 <- pure (CPtr p1)
              update p1 y0
              pure 0
          |]

      let after = [prog|
            grinMain =
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "update" $ do
      let before = [prog|
            grinMain =
              p0 <- store (CInt 0)
              n0 <- pure (CInt 1)
              update p0 n0
              pure 0
          |]

      let after = [prog|
            grinMain =
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "app_simple" $ do
      let before = [prog|
            grinMain =
              y0 <- f 0
              pure 0

            f x = pure 0
          |]

      let after = [prog|
            grinMain =
              pure 0

            f x = pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "app_side_effect_1" $ do
      let before = [prog|
            grinMain =
              n0 <- store (CInt 0)
              p0 <- pure n0
              y0 <- f p0
              y1 <- fetch p0
              pure y1

            f p =
              n1 <- pure (CInt 1)
              update p n1
              pure 0
          |]

      let after = [prog|
            grinMain =
              n0 <- store (CInt 0)
              p0 <- pure n0
              y0 <- f p0
              y1 <- fetch p0
              pure y1

            f p =
              n1 <- pure (CInt 1)
              update p n1
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "app_side_effect_2" $ do
      let before = [prog|
            grinMain =
              x0 <- pure (CInt 0)
              p0 <- store x0
              y0 <- f x0 p0
              y1 <- fetch p0
              pure y1

            f x p =
              p' <- store x
              x' <- fetch p
              pure 0
          |]

      let after = [prog|
            grinMain =
              x0 <- pure (CInt 0)
              p0 <- store x0
              y1 <- fetch p0
              pure y1

            f x p =
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "pattern_match" $ do
      let before = [prog|
            grinMain =
              x <- pure 0
              n <- pure (CNode 0 0)
              p <- store n
              (CNode y z) <- pure n
              n0 <- fetch p
              (CNode v w) <- pure n0
              0 <- pure x
              u <- pure ()
              () <- pure u
              pure 0
          |]

      let after = [prog|
            grinMain =
              x <- pure 0
              n <- pure (CNode 0 0)
              p <- store n
              (CNode y z) <- pure n
              n0 <- fetch p
              (CNode v w) <- pure n0
              0 <- pure x
              u <- pure ()
              () <- pure u
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    -- TODO: reenable after semantics are defined
    xit "binding_pat_match_failure" $ do
      let before = [prog|
            grinMain =
              n <- pure (CNode 0 0)
              (CUnit) <- pure n
              pure 0
          |]

      let after = before

      pipelineSrc before after deadVariableEliminationPipeline

    -- TODO: reenable after semantics are defined
    xit "case_node_pat_failure" $ do
      let before = [prog|
            grinMain =
              x <- pure (CInt 5)
              y <- case x of
                (CUnit) -> pure 5
              pure 0
          |]

      let after = before

      pipelineSrc before after deadVariableEliminationPipeline

    -- NOTE: literals are not tracked
    -- TODO: reenable after semantics are defined
    xit "case_lit_pat_failure" $ do
      let before = [prog|
            grinMain =
              x <- pure 5
              y <- case x of
                5 -> pure 5
              pure 0
          |]

      let after = before

      pipelineSrc before after deadVariableEliminationPipeline

    it "case_pat_default" $ do
      let before = [prog|
            grinMain =
              x <- pure (CInt 5)
              y <- case x of
                (CUnit) -> pure 5
                #default -> pure 3
              pure 0
          |]

      let after = [prog|
            grinMain = pure 0
          |]

      pipelineSrc before after deadVariableEliminationPipeline

    -- NOTE: literals are not tracked, but bool patterns are checked
    it "case_bool_pat_covered" $ do
      let before = [prog|
            grinMain =
              x <- pure #True
              y <- case x of
                #True  -> pure 5
                #False -> pure 3
              pure 0
          |]

      let after = [prog|
            grinMain = pure 0
          |]

      pipelineSrc before after deadVariableEliminationPipeline

    it "case_pat_side_effect" $ do
      let before = [prog|
            grinMain =
              x <- pure (CInt 5)
              y <- case x of
                (CInt n) ->
                  _prim_int_print 5
                  pure 5
              pure 0
          |]

      let after = before

      pipelineSrc before after deadVariableEliminationPipeline

    -- NOTE: weird indentation rules
    it "case_nested_side_effect" $ do
      let before = [prog|
            grinMain =
              x <- pure (CInt 5)
              y <- case x of
                (CInt n) ->
                  z <- case n of
                    #default ->
                      _prim_int_print 5
                      pure ()
                  pure z
              pure 0
          |]

      let after = before

      pipelineSrc before after deadVariableEliminationPipeline

    -- TODO: reenable after semantics are defined
    xit "case_nested_pat_failure" $ do
      let before = [prog|
            grinMain =
              x <- pure (CInt 5)
              y <- case x of
                (CInt n) -> case n of
                              0 -> pure 0
              pure 0
          |]

      let after = before

      pipelineSrc before after deadVariableEliminationPipeline

    it "case_pure_covered_pats" $ do
      let before = [prog|
            grinMain =
              x <- case 0 of
                0 -> pure (CInt 5)
                #default -> pure (CUnit)
              y <- case x of
                (CInt n) -> pure 0
                (CUnit) -> pure 0
              pure 0
          |]

      let after = [prog|
            grinMain = pure 0
          |]

      pipelineSrc before after deadVariableEliminationPipeline

    it "case_nested_pure_covered_pats" $ do
      let before = [prog|
            grinMain =
              x <- case 0 of
                0 -> pure (CInt 5)
                #default -> pure (CUnit)
              y <- case x of
                (CInt n) -> case n of
                              #default -> pure 0
                (CUnit) -> pure 0
              pure 0
          |]

      let after = [prog|
            grinMain = pure 0
          |]

      pipelineSrc before after deadVariableEliminationPipeline

    -- NOTE: This test shows that the DVE can break the binding syntax convention
    it "store_fetch_in_dead_alt" $ do
      let before = [prog|
            grinMain =
              case (CNil) of
                (CNil) ->  pure 0
                (CCons x xs) ->
                  p <- store (CInt 5)
                  n <- fetch p
                  n0 <- pure n
                  (CBad) <- pure n0
                  pure 0
          |]

      let after = [prog|
            grinMain =
              case (CNil) of
                (CNil) ->  pure 0
                (CCons x xs) ->
                  (CBad) <- pure (#undefined :: T_Dead)
                  pure 0
          |]

      pipelineSrc before after deadVariableEliminationPipeline

    it "replace_app" $ do
      let before = [prog|
            grinMain =
              x0 <- pure 5
              f x0 5

            f x y = pure y
          |]

      let after = [prog|
            grinMain =
              f (#undefined :: T_Int64) 5

            f x y = pure y
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "replace_case" $ do
      let before = [prog|
            grinMain =
              x0 <- pure 5
              case (CInt x0) of
                (CInt c0) -> pure 0
          |]

      let after = [prog|
            grinMain =
              case (CInt (#undefined :: T_Int64)) of
                (CInt c0) -> pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "replace_case_rec" $ do
      let before = [prog|
            grinMain =
              x0 <- pure 5
              case (CInt x0) of
                (CInt c0) ->
                  n0 <- pure (CPair x0 5)
                  p1 <- store (CPair x0 5)
                  update p1 (CPair x0 5)
                  y0 <- f x0

                  (CPair c1 c2) <- pure n0
                  n1 <- fetch p1
                  (CPair x1 x2) <- pure n1
                  n2 <- pure (CLive c2 x2 y0)
                  pure n2

            f x = pure 0
          |]

      let after = [prog|
            grinMain =
              case (CInt (#undefined :: T_Int64)) of
                (CInt c0) ->
                  n0 <- pure (CPair (#undefined :: T_Int64) 5)
                  p1 <- store (CPair (#undefined :: T_Int64) 5)
                  update p1 (CPair (#undefined :: T_Int64) 5)
                  y0 <- f (#undefined :: T_Int64)

                  (CPair c1 c2) <- pure n0
                  n1 <- fetch p1
                  (CPair x1 x2) <- pure n1
                  n2 <- pure (CLive c2 x2 y0)
                  pure n2

            f x = pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "replace_pure" $ do
      let before = [prog|
            grinMain =
              x0 <- pure 5
              n0 <- pure (CPair x0 5)
              case n0 of
                (CPair c0 c1) -> pure c1
          |]

      let after = [prog|
            grinMain =
              n0 <- pure (CPair (#undefined :: T_Int64) 5)
              case n0 of
                (CPair c0 c1) -> pure c1
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "replace_store" $ do
      let before = [prog|
            grinMain =
              x0 <- pure 5
              p0 <- store (CPair x0 5)
              n0 <- fetch p0
              (CPair c0 c1) <- pure n0
              pure c1
          |]

      let after = [prog|
            grinMain =
              p0 <- store (CPair (#undefined :: T_Int64) 5)
              n0 <- fetch p0
              (CPair c0 c1) <- pure n0
              pure c1
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "replace_update" $ do
      let before = [prog|
            grinMain =
              x0 <- pure 5
              p0 <- store (CPair 0 0)
              update p0 (CPair x0 5)
              n0 <- fetch p0
              (CPair c0 c1) <- pure n0
              pure c1
          |]

      let after = [prog|
            grinMain =
              p0 <- store (CPair 0 0)
              update p0 (CPair (#undefined :: T_Int64) 5)
              n0 <- fetch p0
              (CPair c0 c1) <- pure n0
              pure c1
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "replace_unspec_loc" $ do
      let before = [prog|
            grinMain =
              x0 <- pure (#undefined :: #ptr)
              n0 <- pure (CPair x0 5)
              case n0 of
                (CPair c0 c1) -> pure c1
          |]

      let after = [prog|
            grinMain =
              n0 <- pure (CPair (#undefined :: #ptr) 5)
              case n0 of
                (CPair c0 c1) -> pure c1
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "true_side_effect_min" $ do
      let before = [prog|
            grinMain =
              result_main <- Main.main1 $
              pure ()

            Main.main1 =
              _prim_int_print $ 1
          |]

      let after = [prog|
            grinMain =
              result_main <- Main.main1 $
              pure ()

            Main.main1 =
              _prim_int_print $ 1
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "true_side_effect_case" $ do
      let before = [prog|
            grinMain =
              a <- pure 0
              b <- pure 0
              c <- pure b
              d <- case c of
                    0 ->
                      e <- _prim_int_add a 0
                      _prim_int_print 0
              pure 0
          |]

      let after = [prog|
            grinMain =
              b <- pure 0
              c <- pure b
              d <- case c of
                    0 ->
                      _prim_int_print 0
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "true_side_effect_fun" $ do
      let before = [prog|
            grinMain =
              a <- pure 0
              b <- pure 0
              c <- f a b
              pure 0

            f x y = _prim_int_print x
          |]

      let after = [prog|
            grinMain =
              a <- pure 0
              c <- f a (#undefined :: T_Int64)
              pure 0

            f x y = _prim_int_print x
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "true_side_effect_case_fun" $ do
      let before = [prog|
            grinMain =
              a <- pure 0
              b <- pure 0
              c <- pure b
              d <- case c of
                    0 ->
                      e <- _prim_int_add a 0
                      f 0 e
              pure 0

            f x y = _prim_int_print x
          |]

      let after = [prog|
            grinMain =
              b <- pure 0
              c <- pure b
              d <- case c of
                    0 ->
                      f 0 (#undefined :: T_Int64)
              pure 0

            f x y = _prim_int_print x
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "true_side_effect_case_nodes_1" $ do
      let before = [prog|
            grinMain =
              a <- pure (COne 0)
              b <- pure (COne)
              c <- pure (CTwo)
              d <- case a of
                (COne n) -> pure b
                (CTwo m) -> pure c
              e <- case d of
                (COne) -> _prim_int_print 0
                (CTwo) -> _prim_int_print 0
              pure 0
          |]

      let after = [prog|
            grinMain =
              a <- pure (COne 0)
              b <- pure (COne)
              d <- case a of
                (COne n) -> pure b
                (CTwo m) -> pure (#undefined :: {CTwo[]})
              e <- case d of
                (COne) -> _prim_int_print 0
                (CTwo) -> _prim_int_print 0
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    it "true_side_effect_case_nodes_2" $ do
      let before = [prog|
            grinMain =
              a <- pure (COne 0)
              d <- case a of
                (COne n) ->
                  b <- pure (COne)
                  pure b
                (CTwo m) ->
                  c <- pure (CTwo)
                  pure c
              e <- case d of
                (COne) -> _prim_int_print 0
                (CTwo) -> _prim_int_print 0
              pure 0
          |]

      let after = [prog|
            grinMain =
              a <- pure (COne 0)
              d <- case a of
                (COne n) ->
                  b <- pure (COne)
                  pure b
                (CTwo m) ->
                  pure (#undefined :: T_Dead)
              e <- case d of
                (COne) -> _prim_int_print 0
                (CTwo) -> _prim_int_print 0
              pure 0
          |]
      pipelineSrc before after deadVariableEliminationPipeline

    describe "bugs" $ do
      xit "Not explicitly covered alternatives trigger undefined replacements" $ do
        let before = [prog|
              grinMain =
                v0 <- _prim_int_add 1 1
                v1 <- case v0 of
                  2 ->
                    v2 <- _prim_int_lt 1 3
                    v3 <- case v2 of
                      #False -> pure v0
                      #True -> pure 1
                    case v3 of
                      0 -> pure (CGT)
                      1 -> pure (CLT)
                  1 -> pure (CEQ)
                -- If #default is changed to explicit alternatives the undefineds are not introduced.
                -- Undefineds are introduced for missing alternatives too.

                {- The real problem here is that liveness information
                   is not correctly propagated back through the case scrutinee.

                   Also, this scenario is even more complicated,
                   because the result of the last case expression is really DEAD,
                   but the alternatives have side effects. This requires further discussion.
                -}
                case v1 of
                  (CEQ) -> _prim_int_print 1
                  #default -> _prim_int_print 2
            |]
        let after = [prog|
              grinMain =
                v0 <- _prim_int_add 1 1
                v1 <- case v0 of
                  2 ->
                    v2 <- _prim_int_lt 1 3
                    v3 <- case v2 of
                      #False -> pure v0
                      #True -> pure 1
                    case v3 of
                      0 -> pure (CGT)
                      1 -> pure (CLT)
                  1 -> pure (CEQ)
                case v1 of
                  (CEQ) -> _prim_int_print 1
                  #default -> _prim_int_print 2
            |]
        result <- P.pipeline defaultOpts Nothing before
          [ T ProducerNameIntroduction
          , T DeadVariableElimination
          , T BindNormalisation
          ]
        result `sameAs` after
