{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module AbstractInterpretation.ExtendedSyntax.CreatedBySpec (spec, calcCByResult) where

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Grin.ExtendedSyntax.Grin hiding (SimpleType(..))
import Grin.ExtendedSyntax.TH

import Test.Hspec
import Test.ExtendedSyntax.Util
import Test.ExtendedSyntax.Assertions

import AbstractInterpretation.ExtendedSyntax.Reduce
import AbstractInterpretation.ExtendedSyntax.CreatedBy.CodeGen
import AbstractInterpretation.ExtendedSyntax.CreatedBy.Result
import AbstractInterpretation.ExtendedSyntax.CreatedBy.Readback
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result as HPT


{- NOTE: Variables with names like "z<i>" are introduced just for naming.
   They are not relevant to the result of the analysis.

   Variables with names like "_<i>" are introduced just for named bindings.
   They will never be used.
-}

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  let calcProducers = _producers . calcCByResult
      calcHPTResultWithCBy = _hptResult . calcCByResult
      mkProducerSet = ProducerSet . M.fromList . map (\(t,xs) -> (t,S.fromList xs))
      emptyProducerSet = mkProducerSet []

  describe "Created-By producers are calculated correctly for" $ do
    it "pures" $ do
      let exp = [prog|
            grinMain =
                z0 <- pure 0
                a <- pure (CInt z0)
                b <- pure a
                c <- pure b
                pure a
        |]
      let producerA = mkProducerSet [(Tag C "Int", ["a"])]
      let puresExpected = ProducerMap $
            M.fromList
              [ ("a", producerA)
              , ("b", producerA)
              , ("c", producerA)

              , ("z0", emptyProducerSet)
              ]
      (calcProducers exp) `shouldBe` puresExpected

    it "function_calls" $ do
      let exp = [prog|
              grinMain =
                z0 <- pure 0
                a <- pure (CInt z0)
                b <- pure a
                c <- f z0
                d <- g z0
                pure 5

              f x =
                z1 <- pure 0
                x1 <- pure (CInt z1)
                pure x1

              g y =
                y1 <- f y
                pure y1
            |]
      let producerA  = mkProducerSet [(Tag C "Int", ["a"])]
          producerX1 = mkProducerSet [(Tag C "Int", ["x1"])]
          expected = ProducerMap $
            M.fromList [ ("a",  producerA)
                       , ("b",  producerA)
                       , ("c",  producerX1)
                       , ("d",  producerX1)
                       , ("x",  emptyProducerSet)
                       , ("x1", producerX1)
                       , ("y",  emptyProducerSet)
                       , ("y1", producerX1)

                       , ("z0",  emptyProducerSet)
                       , ("z1",  emptyProducerSet)
                       ]
      (calcProducers exp) `shouldBe` expected

    it "case_simple" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 0
              a <- f z0
              pure a

            f x =
              z1 <- pure 0
              case x of
                0 @ _1 ->
                  x0 <- pure (CInt z1)
                  pure x0
                1 @ _2 ->
                  x1 <- pure (CBool z1)
                  pure x1
           |]
      let expected = ProducerMap $
            M.fromList [ ("a",  producerA)
                       , ("x",  emptyProducerSet)
                       , ("x0", producerX0)
                       , ("x1", producerX1)

                       , ("z0",  emptyProducerSet)
                       , ("z1",  emptyProducerSet)
                       , ("_1",  emptyProducerSet)
                       , ("_2",  emptyProducerSet)
                       ]
          producerA  = mkProducerSet [ (Tag C "Int",  ["x0"])
                                     , (Tag C "Bool", ["x1"])
                                     ]
          producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
          producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
      (calcProducers exp) `shouldBe` expected

    it "heap" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 0
              x0 <- pure (CInt  z0)
              x1 <- pure (CBool z0)
              x2 <- pure (CBool z0)
              p0 <- store x0
              p1 <- store x1
              _1 <- update p0 x2
              _2 <- update p1 x2
              y0 <- fetch p0
              y1 <- fetch p1
              pure 5
          |]
      let expected = ProducerMap $
            M.fromList [ ("x0", producerX0)
                       , ("x1", producerX1)
                       , ("x2", producerX2)
                       , ("p0", emptyProducerSet)
                       , ("p1", emptyProducerSet)
                       , ("y0", producerY0)
                       , ("y1", producerY1)

                       , ("z0", emptyProducerSet)
                       , ("_1", emptyProducerSet)
                       , ("_2", emptyProducerSet)
                       ]
          producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
          producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
          producerX2 = mkProducerSet [(Tag C "Bool", ["x2"])]
          producerY0 = producerX0 <> producerX2
          producerY1 = producerX1 <> producerX2
      (calcProducers exp) `shouldBe` expected

    it "pointer_in_node" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 0
              n0 <- pure (CNil)
              p0 <- store n0
              n1 <- pure (CCons z0 p0)
              case n1 of
                (CCons x pxs) @ _1 ->
                  xs <- fetch pxs
                  pure 5
          |]
      let expected = ProducerMap $
            M.fromList [ ("n0",  producerN0)
                       , ("p0",  emptyProducerSet)
                       , ("n1",  producerN1)
                       , ("x",   emptyProducerSet)
                       , ("pxs", emptyProducerSet)
                       , ("xs",  producerXS)

                       , ("z0",   emptyProducerSet)
                       , ("_1",   producerN1)
                       ]
          producerN0 = mkProducerSet [(Tag C "Nil",  ["n0"])]
          producerN1 = mkProducerSet [(Tag C "Cons", ["n1"])]
          producerXS = producerN0
      (calcProducers exp)` shouldBe` expected

    it "case_restricted_1" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 0
              a0 <- f z0
              r0 <- case a0 of
                (CInt c0) @ _1  ->
                  b0 <- pure (CInt  z0)
                  pure b0
                (CBool c1) @ _2 ->
                  b1 <- pure (CBool z0)
                  pure b1
                (CNope c2) @ _3 ->
                  b2 <- pure (CNope z0)
                  pure b2
              pure r0


            f x =
              z1 <- pure 0
              case x of
                0 @ _4 ->
                  x0 <- pure (CInt  z0)
                  pure x0
                1 @ _5 ->
                  x1 <- pure (CBool z0)
                  pure x1
          |]
      let expected = ProducerMap $
            M.fromList [ ("a0", producerA0)
                       , ("r0", producerR0)
                       , ("b0", producerB0)
                       , ("b1", producerB1)
                       , ("b2", emptyProducerSet)
                       , ("c0", emptyProducerSet)
                       , ("c1", emptyProducerSet)
                       , ("c2", emptyProducerSet)
                       , ("x",  emptyProducerSet)
                       , ("x0", producerX0)
                       , ("x1", producerX1)

                       , ("z0", emptyProducerSet)
                       , ("z1", emptyProducerSet)
                       , ("_1", producerX0)
                       , ("_2", producerX1)
                       , ("_3", emptyProducerSet)
                       , ("_4", emptyProducerSet)
                       , ("_5", emptyProducerSet)
                       ]
          producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
          producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
          producerA0 = producerX0 <> producerX1
          producerB0 = mkProducerSet [(Tag C "Int",  ["b0"])]
          producerB1 = mkProducerSet [(Tag C "Bool", ["b1"])]
          producerR0 = producerB0 <> producerB1
      (calcProducers exp) `shouldBe` expected

    it "case_restricted_2" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 0
              a0 <- f z0
              r0 <- case a0 of
                (CInt c0) @ _1  ->
                  b0 <- f z0
                  pure b0
                (CBool c1) @ _2 ->
                  b1 <- pure (CBool z0)
                  pure b1
                (CNope c2) @ _3 ->
                  b2 <- pure (CNope z0)
                  pure b2
              pure r0

            f x =
              z1 <- pure 0
              case x of
                0 @ _4 ->
                  x0 <- pure (CInt z1)
                  pure x0
                1 @ _5 ->
                  x1 <- pure (CBool z1)
                  pure x1
          |]
      let expected = ProducerMap $
            M.fromList [ ("a0", producerA0)
                       , ("r0", producerR0)
                       , ("b0", producerB0)
                       , ("b1", producerB1)
                       , ("b2", emptyProducerSet)
                       , ("c0", emptyProducerSet)
                       , ("c1", emptyProducerSet)
                       , ("c2", emptyProducerSet)
                       , ("x",  emptyProducerSet)
                       , ("x0", producerX0)
                       , ("x1", producerX1)

                       , ("z0", emptyProducerSet)
                       , ("z1", emptyProducerSet)
                       , ("_1", producerX0)
                       , ("_2", producerX1)
                       , ("_3", emptyProducerSet)
                       , ("_4", emptyProducerSet)
                       , ("_5", emptyProducerSet)
                       ]
          producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
          producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
          producerA0 = producerX0 <> producerX1
          producerB0 = producerX0 <> producerX1
          producerB1 = mkProducerSet [(Tag C "Bool", ["b1"])]
          producerR0 = producerB0 <> producerB1
      (calcProducers exp) `shouldBe` expected

    it "case_restricted_3" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 1
              a0 <- f z0
              a1 <- pure (CWord z0)
              r0 <- case a0 of
                (CInt c0) @ _1 ->
                  b0 <- g a0
                  pure b0
                (CBool c1) @ _2 ->
                  b1 <- g a1
                  pure b1
                (CNope c2) @ _3 ->
                  b2 <- pure (CNope z0)
                  pure b2
              pure r0

            f x =
              z1 <- pure 1
              case x of
                0 @ _4 ->
                  x0 <- pure (CInt  z1)
                  pure x0
                1 @ _5 ->
                  x1 <- pure (CBool z1)
                  pure x1

            g y =
              z2 <- pure 1
              case y of
                (CInt n) @ _6 ->
                  y0 <- pure (CInt  z2)
                  pure y0
                (CBool b) @ _7 ->
                  y1 <- pure (CBool z2)
                  pure y1
                (CWord w) @ _8 ->
                  y2 <- pure (CWord z2)
                  pure y2
          |]
      let restrictedBy (ProducerSet ps) tag = ProducerSet $ M.filterWithKey (\k _ -> k == tag) ps
      let expected = ProducerMap $
            M.fromList [ ("a0", producerA0)
                       , ("a1", producerA1)
                       , ("r0", producerR0)
                       , ("b0", producerB0)
                       , ("b1", producerB1)
                       , ("b2", emptyProducerSet)
                       , ("c0", emptyProducerSet)
                       , ("c1", emptyProducerSet)
                       , ("c2", emptyProducerSet)
                       , ("x",  emptyProducerSet)
                       , ("x0", producerX0)
                       , ("x1", producerX1)
                       , ("y",  producerY)
                       , ("y0", producerY0)
                       , ("y1", emptyProducerSet) -- because the control never reaches it
                       , ("y2", producerY2)
                       , ("n",  emptyProducerSet)
                       , ("b",  emptyProducerSet)
                       , ("w",  emptyProducerSet)

                       , ("z0", emptyProducerSet)
                       , ("z1", emptyProducerSet)
                       , ("z2", emptyProducerSet)
                       , ("_1", producerX0)
                       , ("_2", producerX1)
                       , ("_3", emptyProducerSet)
                       , ("_4", emptyProducerSet)
                       , ("_5", emptyProducerSet)
                       , ("_6", producerX0)
                       , ("_7", emptyProducerSet)
                       , ("_8", producerA1)
                       ]
          producerX0 = mkProducerSet [(Tag C "Int",  ["x0"])]
          producerX1 = mkProducerSet [(Tag C "Bool", ["x1"])]
          producerA0 = producerX0 <> producerX1
          producerA1 = mkProducerSet [(Tag C "Word", ["a1"])]
          producerY  = producerA0 `restrictedBy` (Tag C "Int") <> producerA1
          producerY0 = mkProducerSet [(Tag C "Int",  ["y0"])]
          producerY1 = mkProducerSet [(Tag C "Bool", ["y1"])]
          producerY2 = mkProducerSet [(Tag C "Word", ["y2"])]
          producerB0 = producerY0 <> producerY2 -- because the analysis is not context sensitive
          producerB1 = producerY0 <> producerY2 -- because the analysis is not context sensitive
          producerR0 = producerB0 <> producerB1
      (calcProducers exp) `shouldBe` expected

    it "undefined" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 0
              z1 <- pure (CNil)
              z3 <- pure (#undefined :: T_Int64)

              p0 <- store z1
              z2 <- pure (CCons z0 p0)

              p1 <- store z2
              x0 <- pure (#undefined :: T_Int64)
              n0 <- pure (#undefined :: {CCons[T_Int64,{0,1}]})
              p2 <- store n0
              n1 <- pure (#undefined :: {CNil[],CCons[T_Int64,{2}]})
              n2 <- pure (CCons z3 p0)
              pure 5
          |]
      let expected =ProducerMap $
            M.fromList [ ("n0",  producerN0)
                       , ("n1",  producerN1)
                       , ("n2",  producerN2)
                       , ("p0",  emptyProducerSet)
                       , ("p1",  emptyProducerSet)
                       , ("p2",  emptyProducerSet)
                       , ("x0",  emptyProducerSet)

                       , ("z0", emptyProducerSet)
                       , ("z1", producerZ1)
                       , ("z2", producerZ2)
                       , ("z3", emptyProducerSet)
                       ]
          producerN0 = mkProducerSet [(Tag C "Cons", [undefinedProducerName])]
          producerN1 = mkProducerSet [(Tag C "Cons", [undefinedProducerName]), (Tag C "Nil", [undefinedProducerName])]
          producerN2 = mkProducerSet [(Tag C "Cons", ["n2"])]

          producerZ1 = mkProducerSet [(Tag C "Nil",  ["z1"])]
          producerZ2 = mkProducerSet [(Tag C "Cons", ["z2"])]
      (calcProducers exp) `shouldBe` expected

    it "unspec_loc" $ do
      let exp = [prog|
              grinMain =
                z0 <- pure 0
                n0 <- pure (CNil)
                p0 <- case z0 of
                  0 @ _1 -> store n0
                  1 @ _2 -> pure (#undefined :: #ptr)
                n1 <- fetch p0
                pure 0
            |]
      let expected = ProducerMap $
            M.fromList [ ("n0",  producerN0)
                       , ("n1",  producerN0)
                       , ("p0",  emptyProducerSet)

                       , ("z0",  emptyProducerSet)
                       , ("_1",  emptyProducerSet)
                       , ("_2",  emptyProducerSet)
                       ]
          producerN0 = mkProducerSet [(cNil, ["n0"])]
      (calcProducers exp) `shouldBe` expected

    it "variable copies" $ do
      let exp = [prog|
            grinMain =
                z0 <- pure 0
                a1 <- pure (CInt z0)
                a2 <- pure a1
                b1 <- pure a1
                b2 <- pure a2
                c1 <- pure b1
                c2 <- pure b2
                pure a1
        |]
      let producerA1 = mkProducerSet [(Tag C "Int", ["a1"])]
      let puresExpected = ProducerMap $
            M.fromList
              [ ("a1", producerA1)
              , ("a2", producerA1)
              , ("b1", producerA1)
              , ("b2", producerA1)
              , ("c1", producerA1)
              , ("c2", producerA1)

              , ("z0", emptyProducerSet)
              ]
      (calcProducers exp) `shouldBe` puresExpected

    it "node as-patterns" $ do
      let exp = [prog|
            grinMain =
                z0 <- pure 0
                (CInt _1) @ a <- pure (CInt z0)
                b <- pure a
                pure a
        |]
      let producerA = mkProducerSet [(Tag C "Int", ["a"])]
      let puresExpected = ProducerMap $
            M.fromList
              [ ("a", producerA)
              , ("b", producerA)

              , ("_1", emptyProducerSet)
              , ("z0", emptyProducerSet)
              ]
      (calcProducers exp) `shouldBe` puresExpected

  describe "Created-By type info" $ do

    it "undefined" $ do
      let exp = [prog|
            grinMain =
              z0 <- pure 0
              z1 <- pure (CNil)
              z3 <- pure (#undefined :: T_Int64)

              p0 <- store z1
              z2 <- pure (CCons z0 p0)

              p1 <- store z2
              x0 <- pure (#undefined :: T_Int64)
              n0 <- pure (#undefined :: {CCons[T_Int64,{0,1}]})
              p2 <- store n0
              n1 <- pure (#undefined :: {CNil[],CCons[T_Int64,{2}]})
              n2 <- pure (CCons z3 p0)
              pure 5
          |]
      let expected = HPTResult
            { HPT._memory   = undefinedExpectedHeap
            , HPT._register = undefinedExpectedRegisters
            , HPT._function = undefinedExpectedFunctions
            }
          undefinedExpectedRegisters = M.fromList
            [ ("p0", loc 0)
            , ("p1", loc 1)
            , ("p2", loc 2)
            , ("x0", tySetFromTypes [T_Int64])
            , ("n0", tySetFromNodeSet nodeSetN0)
            , ("n1", typeN1)
            , ("n2", typeN2)

            , ("z0", tySetFromTypes [T_Int64])
            , ("z1", tySetFromNodeSet $ mkNodeSet [ (cNil,  []) ])
            , ("z2", tySetFromNodeSet $ mkNodeSet [ (cCons, [[T_Int64], [T_Location 0]]) ])
            , ("z3", tySetFromTypes [T_Int64])
            ]
            where typeN1 = mkTySet [ (cCons, [[T_Int64], [T_Location 2]])
                                   , (cNil, [])
                                   ]
                  typeN2 = mkTySet [ (cCons, [[T_Int64], [T_Location 0]])
                                   ]
          undefinedExpectedFunctions = M.singleton "grinMain" (mkSimpleMain T_Int64)
          undefinedExpectedHeap = V.fromList
            [ mkNodeSet [(cNil, [])]
            , mkNodeSet [(cCons, [[T_Int64], [T_Location 0]])]
            , nodeSetN0
            ]


          nodeSetN0 = mkNodeSet [(cCons, [[T_Int64], [T_Location 0, T_Location 1]])]
      (calcHPTResultWithCBy exp) `shouldBe` expected

    it "unspec_loc" $ do
      let exp = [prog|
              grinMain =
                z0 <- pure 0
                z1 <- pure (CInt z0)

                p0 <- case z0 of
                  0 @ _1 -> store z1
                  1 @ _2 -> pure (#undefined :: #ptr)
                n0 <- fetch p0
                n1 <- pure (#undefined :: {CNode[#ptr]})
                (CNode p1) @ _3 <- pure n1
                x0 <- fetch p1
                update p0 x0
            |]
      let expected = HPTResult
            { HPT._memory   = unspecLocExpectedHeap
            , HPT._register = unspecLocExpectedRegisters
            , HPT._function = unspecLocExpectedFunctions
            }

          nodeSetN0, nodeSetN1 :: HPT.NodeSet
          nodeSetN0 = mkNodeSet [(cInt,  [[T_Int64]])]
          nodeSetN1 = mkNodeSet [(cNode, [[T_UnspecifiedLocation]])]
          unspecLocExpectedHeap = V.fromList [ nodeSetN0 ]
          unspecLocExpectedRegisters = M.fromList
            [ ("p0", tySetFromTypes [T_Location 0, T_UnspecifiedLocation])
            , ("p1", unspecLoc)
            , ("n0", tySetFromNodeSet nodeSetN0)
            , ("n1", tySetFromNodeSet nodeSetN1)
            , ("x0", tySetFromTypes [])

            , ("z0", tySetFromTypes [T_Int64])
            , ("z1", tySetFromNodeSet $ mkNodeSet [ (cInt,  [ [T_Int64] ]) ])
            , ("_1", tySetFromTypes [T_Int64])
            , ("_2", tySetFromTypes [T_Int64])
            , ("_3", tySetFromNodeSet $ mkNodeSet [ (cNode, [ [T_UnspecifiedLocation] ]) ])
            ]
          unspecLocExpectedFunctions = M.singleton "grinMain" (mkSimpleMain T_Unit)
      (calcHPTResultWithCBy exp) `shouldBe` expected

calcCByResult :: Exp -> CByResult
calcCByResult prog
  | (cbyProgram, cbyMapping) <- codeGen prog
  , computer <- _airComp . evalAbstractProgram $ cbyProgram
  , cbyResult <- toCByResult cbyMapping computer
  = cbyResult
