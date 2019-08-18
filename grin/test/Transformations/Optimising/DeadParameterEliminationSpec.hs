{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.Optimising.DeadParameterEliminationSpec where

import Test.Hspec
import Test.Hspec.PipelineExample
import Pipeline.Pipeline hiding (pipeline)
import Grin.TH


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Dead Parameter Elimination" $ do

    let deadParameterEliminationPipeline =
          [ T DeadParameterElimination
          ]

    it "Fnode" $ do
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
              a0 <- pure (Ffoo p0 p0 p0)
              p1 <- store a0
              a1 <- eval p1
              pure a1

            -- functions cannot return pointers
            foo y =
              z <- pure (#undefined :: #ptr)
              x <- pure (#undefined :: #ptr)
              y' <- eval y
              pure y'

            eval p =
              v <- fetch p
              case v of
                (CInt n) -> pure v
                (Ffoo x1 y1 z1) ->
                  w <- foo y1
                  update p w
                  pure w
          |]
      pipelineSrc before after deadParameterEliminationPipeline

    it "Pnode" $ pipeline
      "dead-parameter-elimination/pnode.grin"
      "dead-parameter-elimination/pnode.grin.expected"
      deadParameterEliminationPipeline

    it "Pnode opt" $ do
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
              fooRet        <- foo v2
              pure fooRet

            foo y0 =
              z0 <- pure (#undefined :: #ptr)
              x0 <- pure (#undefined :: #ptr)
              y0' <- fetch y0
              (CInt n) <- y0'
              pure y0'
          |]
      pipelineSrc before after deadParameterEliminationPipeline

    it "Simple" $ do
      let before = [prog|
            grinMain = g 5

            f x y = pure x

            g z = f 0 z
          |]

      let after = [prog|
            grinMain = g

            f x =
              y <- pure (#undefined :: T_Int64)
              pure x

            g =
              z <- pure (#undefined :: T_Int64)
              f 0
          |]
      pipelineSrc before after deadParameterEliminationPipeline

    it "Mutually recursive" $ do
      let before = [prog|
            grinMain = f 0 0

            f x y = g x 0

            g v w = f 0 w
          |]

      let after = [prog|
            grinMain = f

            f =
              y <- pure (#undefined :: T_Int64)
              x <- pure (#undefined :: T_Int64)
              g

            g =
              w <- pure (#undefined :: T_Int64)
              v <- pure (#undefined :: T_Int64)
              f
          |]
      pipelineSrc before after deadParameterEliminationPipeline
