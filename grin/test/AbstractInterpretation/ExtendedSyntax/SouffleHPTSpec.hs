{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module AbstractInterpretation.ExtendedSyntax.SouffleHPTSpec where

import Test.Hspec
import Test.ExtendedSyntax.Util
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Lint
import Grin.ExtendedSyntax.TypeEnv as TypeEnv
import Grin.ExtendedSyntax.TypeCheck
import Grin.ExtendedSyntax.PrimOpsPrelude
import Test.ExtendedSyntax.Assertions

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result as HPT
import Grin.ExtendedSyntax.SouffleHPT


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do

  let loc = tySetFromTypes . pure . HPT.T_Location
      mkNode = V.fromList . map Set.fromList
      mkNodeSet = NodeSet . Map.fromList . map (\(t,v) -> (t,mkNode v))
      tySetFromNodes = TypeSet mempty
      mkTySet = tySetFromNodes . mkNodeSet
      tySetFromTypes = flip TypeSet mempty . Set.fromList
      mkSimpleMain t = (tySetFromTypes [t], mempty)

  describe "HPT Result" $ do
    xit "undefined" $ do
      let exp = [prog|
            grinMain =
              nil <- pure (CNil)
              p0 <- store nil
              k0 <- pure 0
              cons <- pure (CCons k0 p0)
              p1 <- store cons
              x0 <- pure (#undefined :: T_Int64)
              n0 <- pure (#undefined :: {CCons[T_Int64,{0,1}]})
              p2 <- store n0
              n1 <- pure (#undefined :: {CNil[],CCons[T_Int64,{2}]})
              x1 <- pure (#undefined :: T_Int64)
              n2 <- pure (CCons x1 p0)
              pure 5
          |]
      let expected = HPTResult
            { HPT._memory   = undefinedExpectedHeap
            , HPT._register = undefinedExpectedRegisters
            , HPT._function = Map.singleton "grinMain" (mkSimpleMain HPT.T_Int64)
            }
          nodeSetN0 = mkNodeSet [(cCons, [[HPT.T_Int64], [HPT.T_Location 0, HPT.T_Location 1]])]
          undefinedExpectedHeap = V.fromList
            [ nilTy
            , consTy
            , nodeSetN0
            ]
          undefinedExpectedRegisters = Map.fromList
            [ ("p0",   loc 0)
            , ("p1",   loc 1)
            , ("p2",   loc 2)
            , ("nil",  tySetFromNodes nilTy)
            , ("cons", tySetFromNodes consTy)
            , ("x0",   tySetFromTypes [HPT.T_Int64])
            , ("x1",   tySetFromTypes [HPT.T_Int64])
            , ("n0",   TypeSet mempty nodeSetN0)
            , ("n1",   typeN1)
            , ("n2",   typeN2)
            , ("k0",   tySetFromTypes [HPT.T_Int64])
            ]
          typeN1 = mkTySet [ (cCons, [[HPT.T_Int64], [HPT.T_Location 2]]) , (cNil, []) ]
          typeN2 = mkTySet [ (cCons, [[HPT.T_Int64], [HPT.T_Location 0]]) ]
          nilTy  = mkNodeSet [(cNil, [])]
          consTy = mkNodeSet [(cCons, [[HPT.T_Int64], [HPT.T_Location 0]])]

      Just res <- calculateHPTResult exp
      res `sameAs` expected

    xit "unspec_loc" $ do
      let exp = [prog|
            grinMain =
              k0 <- pure 0
              p0 <- case k0 of
                0 @ _1 ->
                  nil <- pure (CNil)
                  store nil
                1 @ _2 ->
                  pure (#undefined :: #ptr)
              n0 <- fetch p0
              n1 <- pure (#undefined :: {CNode[#ptr]})
              (CNode p1)@_v <- pure n1
              x0 <- fetch p1
              update p0 x0
          |]
      let expected = HPTResult
            { HPT._memory   = V.fromList [ nodeSetN0 ]
            , HPT._register = unspecLocExpectedRegisters
            , HPT._function = Map.singleton "grinMain" (mkSimpleMain HPT.T_Unit)
            }
          nodeSetN0 = mkNodeSet [(cNil,  [])]
          nodeSetN1 = mkNodeSet [(cNode, [[HPT.T_UnspecifiedLocation]])]

          unspecLocExpectedRegisters = Map.fromList
            [ ("k0", tySetFromTypes [HPT.T_Int64])
            , ("p0", tySetFromTypes [HPT.T_Location 0, HPT.T_UnspecifiedLocation])
            , ("p1", tySetFromTypes [HPT.T_UnspecifiedLocation])
            , ("n0", tySetFromNodes nodeSetN0)
            , ("n1", tySetFromNodes nodeSetN1)
            , ("_v", tySetFromNodes nodeSetN1)
            , ("x0", tySetFromTypes [])
            , ("nil", tySetFromNodes nodeSetN0)

            , ("_1", tySetFromTypes [HPT.T_Int64])
            , ("_2", tySetFromTypes [HPT.T_Int64])
            ]
      Just res <- calculateHPTResult exp
      res `sameAs` expected

    it "simple_case_node" $ do
      let exp = [prog|
            grinMain =
              k0 <- pure 0
              p0 <- case k0 of
                0 @ _1 ->
                  one <- pure (COne)
                  a1 <- store one
                  pure a1
                1 @ _2 ->
                  two <- pure (CTwo)
                  a2 <- store two
                  pure a2
              n0 <- fetch p0
              g1 <- case n0 of
                (COne) @ _3 ->
                  a3 <- pure 1
                  pure a3
                (CTwo) @ _4 ->
                  a4 <- pure 2
                  pure a4
              pure g1
          |]
      let expected = HPTResult
            { HPT._memory   = V.fromList [ nodeSetOne, nodeSetTwo ]
            , HPT._register = unspecLocExpectedRegisters
            , HPT._function = mempty
            }
          nodeSetOne = mkNodeSet [(cOne,  [])]
          nodeSetTwo = mkNodeSet [(cTwo,  [])]

          unspecLocExpectedRegisters = Map.fromList
            [ ("k0", tySetFromTypes [HPT.T_Int64])
            , ("p0", tySetFromTypes [HPT.T_Location 0, HPT.T_Location 1])
            , ("n0", tySetFromNodes (nodeSetOne <> nodeSetTwo))
            , ("a1", tySetFromTypes [HPT.T_Location 0])
            , ("a2", tySetFromTypes [HPT.T_Location 1])
            , ("a3", tySetFromTypes [HPT.T_Int64])
            , ("a4", tySetFromTypes [HPT.T_Int64])
            , ("g1", tySetFromTypes [HPT.T_Int64])
            , ("one", tySetFromNodes nodeSetOne)
            , ("two", tySetFromNodes nodeSetTwo)

            , ("_1", tySetFromTypes [HPT.T_Int64])
            , ("_2", tySetFromTypes [HPT.T_Int64])
            , ("_3", tySetFromNodes nodeSetOne)
            , ("_4", tySetFromNodes nodeSetTwo)
            ]
      Just res <- calculateHPTResult exp
      res `sameAs` expected

    it "handles as-patterns with nodes correctly" $ do
      let exp = withPrimPrelude [prog|
            grinMain =
              k0 <- pure 0
              x0 <- pure (CInt k0)
              (CInt k1)@x1 <- pure x0
              k2 <- _prim_int_add k0 k1
              _v <- _prim_int_print k2
              pure _v
          |]
      let expected = HPTResult
            { HPT._memory   = mempty
            , HPT._register = Map.fromList
                [ ("k0", tySetFromTypes [HPT.T_Int64])
                , ("x0", tySetFromNodes (mkNodeSet [(cInt, [[HPT.T_Int64]])]))
                , ("x1", tySetFromNodes (mkNodeSet [(cInt, [[HPT.T_Int64]])]))
                , ("k1", tySetFromTypes [HPT.T_Int64])
                , ("k2", tySetFromTypes [HPT.T_Int64])
                , ("_v", tySetFromTypes [HPT.T_Unit])
                ]
            , HPT._function = Map.fromList
                [ ("_prim_int_add", (tySetFromTypes [HPT.T_Int64], V.fromList [tySetFromTypes [HPT.T_Int64], tySetFromTypes [HPT.T_Int64]]))
                , ("_prim_int_print", (tySetFromTypes [HPT.T_Unit], V.fromList [tySetFromTypes [HPT.T_Int64]]))
                ]
            }
      Just res <- calculateHPTResult exp
      res `sameAs` expected
