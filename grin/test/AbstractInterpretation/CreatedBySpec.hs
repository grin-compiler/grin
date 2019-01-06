{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module AbstractInterpretation.CreatedBySpec (spec, calcCByResult) where

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import System.FilePath

import Grin.Grin hiding (SimpleType(..))
import Grin.TH

import Test.IO
import Test.Test
import Test.Util
import Test.Hspec
import Test.Assertions

import AbstractInterpretation.IR hiding (Tag)
import AbstractInterpretation.Reduce
import AbstractInterpretation.CreatedBy.CodeGen
import AbstractInterpretation.CreatedBy.Result
import AbstractInterpretation.CreatedBy.Readback
import AbstractInterpretation.HeapPointsTo.Result as HPT



runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  let calcProducers = _producers . calcCByResult
      calcHPTResultWithCBy = _hptResult . calcCByResult
      mkProducerSet = ProducerSet . M.fromList . map (\(t,xs) -> (t,S.fromList xs))
      emptyProducerSet = mkProducerSet []
      unspecLoc = tySetFromTypes [T_UnspecifiedLocation]
      loc = tySetFromTypes . pure . T_Location
      mkNode = V.fromList . map S.fromList
      mkNodeSet = HPT.NodeSet . M.fromList . map (\(t,v) -> (t,mkNode v))
      mkTySet = tySetFromNodeSet . mkNodeSet
      tySetFromNodeSet = TypeSet mempty
      tySetFromTypes = flip TypeSet mempty . S.fromList
      mkSimpleMain t = (tySetFromTypes [t], mempty)

  describe "Created-By producers are calculated correctly for" $ do
    it "pures" $ do
      let exp = [prog|
              grinMain =
                a <- pure (CInt 5)
                b <- pure a
                c <- pure b
                pure c
            |]
      let producerA = mkProducerSet [(Tag C "Int", ["a"])]
      let puresExpected = ProducerMap $
            M.fromList
              [ ("a", producerA)
              , ("b", producerA)
              , ("c", producerA)
              ]
      (calcProducers exp) `shouldBe` puresExpected

    it "function_call" $ do
      let exp = [prog|
              grinMain =
                a <- pure (CInt 5)
                b <- pure a
                c <- f 5
                d <- g 5
                pure 5

              f x =
                x1 <- pure (CInt 5)
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
                       ]
      (calcProducers exp) `shouldBe` expected

    it "case_simple" $ do
      let exp = [prog|
            grinMain =
              a <- f 0
              pure a

            f x =
             case x of
              0 -> x0 <- pure (CInt 5)
                   pure x0
              1 -> x1 <- pure (CBool 0)
                   pure x1
           |]
      let expected = ProducerMap $
            M.fromList [ ("a",  producerA)
                       , ("x",  emptyProducerSet)
                       , ("x0", producerX0)
                       , ("x1", producerX1)
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
              x0 <- pure (CInt 5)
              x1 <- pure (CBool 0)
              x2 <- pure (CBool 1)
              p0 <- store x0
              p1 <- store x1
              update p0 x2
              update p1 x2
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
              n0 <- pure (CNil)
              p0 <- store n0
              n1 <- pure (CCons 5 p0)
              case n1 of
                (CCons x pxs) -> xs <- fetch pxs
                                 pure 5
          |]
      let expected = ProducerMap $
            M.fromList [ ("n0",  producerN0)
                       , ("p0",  emptyProducerSet)
                       , ("n1",  producerN1)
                       , ("x",   emptyProducerSet)
                       , ("pxs", emptyProducerSet)
                       , ("xs",  producerXS)
                       ]
          producerN0 = mkProducerSet [(Tag C "Nil",  ["n0"])]
          producerN1 = mkProducerSet [(Tag C "Cons", ["n1"])]
          producerXS = producerN0
      (calcProducers exp)` shouldBe` expected

    it "case_restricted_1" $ do
      let exp = [prog|
            grinMain =
              a0 <- f 0
              r0 <- case a0 of
                (CInt c0)  -> b0 <- pure (CInt 5)
                              pure b0
                (CBool c1) -> b1 <- pure (CBool 0)
                              pure b1
                (CNope c2) -> b2 <- pure (CNope 1)
                              pure b2
              pure r0


            f x =
             case x of
              0 -> x0 <- pure (CInt 5)
                   pure x0
              1 -> x1 <- pure (CBool 0)
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
              a0 <- f 0
              r0 <- case a0 of
                (CInt c0)  -> b0 <- f 0
                              pure b0
                (CBool c1) -> b1 <- pure (CBool 0)
                              pure b1
                (CNope c2) -> b2 <- pure (CNope 1)
                              pure b2
              pure r0

            f x =
             case x of
              0 -> x0 <- pure (CInt 5)
                   pure x0
              1 -> x1 <- pure (CBool 0)
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
              a0 <- f 1
              a1 <- pure (CWord 3)
              r0 <- case a0 of
                (CInt c0)  -> b0 <- g a0
                              pure b0
                (CBool c1) -> b1 <- g a1
                              pure b1
                (CNope c2) -> b2 <- pure (CNope 1)
                              pure b2
              pure r0

            f x =
             case x of
              0 -> x0 <- pure (CInt 5)
                   pure x0
              1 -> x1 <- pure (CBool 0)
                   pure x1

            g y =
             case y of
              (CInt n)  -> y0 <- pure (CInt 5)
                           pure y0
              (CBool b) -> y1 <- pure (CBool 0)
                           pure y1
              (CWord w) -> y2 <- pure (CWord 3)
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
              p0 <- store (CNil)
              p1 <- store (CCons 0 p0)
              x0 <- pure (#undefined :: T_Int64)
              n0 <- pure (#undefined :: {CCons[T_Int64,{0,1}]})
              p2 <- store n0
              n1 <- pure (#undefined :: {CNil[],CCons[T_Int64,{2}]})
              n2 <- pure (CCons (#undefined :: T_Int64) p0)
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
                       ]
          producerN0 = mkProducerSet [(Tag C "Cons", [undefinedProducerName])]
          producerN1 = mkProducerSet [(Tag C "Cons", [undefinedProducerName]), (Tag C "Nil", [undefinedProducerName])]
          producerN2 = mkProducerSet [(Tag C "Cons", ["n2"])]
      (calcProducers exp) `shouldBe` expected

    it "unspec_loc" $ do
      let exp = [prog|
              grinMain =
                n0 <- pure (CNil)
                p0 <- case 0 of
                  0 -> store n0
                  1 -> pure (#undefined :: #ptr)
                n1 <- fetch p0
                pure 0
            |]
      let expected = ProducerMap $
            M.fromList [ ("n0",  producerN0)
                       , ("n1",  producerN0)
                       , ("p0",  emptyProducerSet)
                       ]
          producerN0 = mkProducerSet [(cNil, ["n0"])]
      (calcProducers exp) `shouldBe` expected

  describe "Created-By type info" $ do

    it "undefined" $ do
      let exp = [prog|
            grinMain =
              p0 <- store (CNil)
              p1 <- store (CCons 0 p0)
              x0 <- pure (#undefined :: T_Int64)
              n0 <- pure (#undefined :: {CCons[T_Int64,{0,1}]})
              p2 <- store n0
              n1 <- pure (#undefined :: {CNil[],CCons[T_Int64,{2}]})
              n2 <- pure (CCons (#undefined :: T_Int64) p0)
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
                p0 <- case 0 of
                  0 -> store (CInt 5)
                  1 -> pure (#undefined :: #ptr)
                n0 <- fetch p0
                n1 <- pure (#undefined :: {CNode[#ptr]})
                (CNode p1) <- pure n1
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
            ]
          unspecLocExpectedFunctions = M.singleton "grinMain" (mkSimpleMain T_Unit)
      (calcHPTResultWithCBy exp) `shouldBe` expected

calcCByResult :: Exp -> CByResult
calcCByResult prog
  | (cbyProgram, cbyMapping) <- codeGen prog
  , computer <- _airComp . evalAbstractProgram $ cbyProgram
  , cbyResult <- toCByResult cbyMapping computer
  = cbyResult
