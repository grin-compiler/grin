{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module AbstractInterpretation.ExtendedSyntax.HptSpec where

import Test.Hspec
import Test.ExtendedSyntax.Util
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Lint
import Grin.ExtendedSyntax.TypeEnv as TypeEnv
import Grin.ExtendedSyntax.TypeCheck
import Grin.ExtendedSyntax.PrimOpsPrelude

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result as HPT
import AbstractInterpretation.ExtendedSyntax.Reduce (evalAbstractProgram, _airComp)
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGen (codeGen)

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "HPT" $ do
    it "calculates nondefined functions" $ do
      let code = [prog|
        grinMain =
          a <- pure 1
          b <- nondeffun a
          pure ()
        |]
      let result = inferTypeEnv code
      let exptected = emptyTypeEnv
            { _variable = Map.fromList
                [ ("a", int64_t)
                , ("b", dead_t)
                ]
            , TypeEnv._function = mconcat
                [ fun_t "grinMain" [] unit_t
                , fun_t "nondeffun" [int64_t] dead_t
                ]
            }
      result `shouldBe` exptected

    it "handles as-patterns with nodes correctly" $ do
      let code = withPrimPrelude [prog|
            grinMain =
              k0 <- pure 0
              x0 <- pure (CInt k0)
              (CInt k1)@x1 <- pure x0
              k2 <- _prim_int_add k0 k1
              _v <- _prim_int_print k2
              pure ()
          |]
      let result = inferTypeEnv code
          exptected = emptyTypeEnv
            { TypeEnv._variable = Map.fromList
                [ ("k0",   int64_t)
                , ("k1",   int64_t)
                , ("k2",   int64_t)
                , ("_v",   unit_t)
                , ("x0",   T_NodeSet $ cnode_t "Int" [TypeEnv.T_Int64])
                , ("x1",   T_NodeSet $ cnode_t "Int" [TypeEnv.T_Int64])
                ]
            , TypeEnv._function = mconcat
                [ fun_t "grinMain" [] unit_t
                , fun_t "_prim_int_add" [int64_t, int64_t] int64_t
                , fun_t "_prim_int_print" [int64_t] unit_t
                ]
            }
      result `shouldBe` exptected

  describe "case" $ do
    it "default bug01" $ do
      let code = withPrimPrelude [prog|
        grinMain =
          n <- pure 1
          m <- pure 2
          (CInt x)@_v <- test n m
          y <- pure x
          _prim_int_print y

        test a b =
          c <- _prim_int_add a b
          case c of
            0 @ _1 ->
              k <- pure 100
              pure (CInt k)
            1 @ _2 ->
              e0 <- pure c
              pure (CInt e0)
            #default@_3 ->
              e1 <- pure c
              pure (CInt e1)
        |]
      let result = inferTypeEnv code
          exptected = emptyTypeEnv
            { TypeEnv._variable = Map.fromList
                [ ("a",   int64_t)
                , ("b",   int64_t)
                , ("c",   int64_t)
                , ("e0",  int64_t)
                , ("e1",  int64_t)
                , ("x",   int64_t)
                , ("y",   int64_t)
                , ("n",   int64_t)
                , ("m",   int64_t)
                , ("k",   int64_t)
                , ("_v",  T_NodeSet $ cnode_t "Int" [TypeEnv.T_Int64])

                , ("_1", int64_t)
                , ("_2", int64_t)
                , ("_3", int64_t)
                ]
            , TypeEnv._function = mconcat
                [ fun_t "_prim_int_add" [int64_t, int64_t] int64_t
                , fun_t "_prim_int_print" [int64_t] unit_t
                , fun_t "grinMain" [] unit_t
                , fun_t "test" [int64_t, int64_t] $ T_NodeSet $ cnode_t "Int" [TypeEnv.T_Int64]
                ]
            }
      result `shouldBe` exptected

  let loc = tySetFromTypes . pure . HPT.T_Location
      mkNode = V.fromList . map Set.fromList
      mkNodeSet = NodeSet . Map.fromList . map (\(t,v) -> (t,mkNode v))
      tySetFromNodes = TypeSet mempty
      mkTySet = tySetFromNodes . mkNodeSet
      tySetFromTypes = flip TypeSet mempty . Set.fromList
      mkSimpleMain t = (tySetFromTypes [t], mempty)

  describe "HPT Result" $ do
    it "undefined" $ do
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

      (calcHPTResult exp) `shouldBe` expected

    it "unspec_loc" $ do
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
      (calcHPTResult exp) `shouldBe` expected

    it "simple_case_node" $ do
      let exp = [prog|
            grinMain =
              k0 <- pure 0
              p0 <- case k0 of
                0 @ _1 ->
                  one <- pure (COne)
                  store one
                1 @ _2 ->
                  two <- pure (CTwo)
                  store two
              n0 <- fetch p0
              case n0 of
                (COne) @ _3 -> pure ()
                (CTwo) @ _4 -> pure ()
          |]
      let expected = HPTResult
            { HPT._memory   = V.fromList [ nodeSetOne, nodeSetTwo ]
            , HPT._register = unspecLocExpectedRegisters
            , HPT._function = Map.singleton "grinMain" (mkSimpleMain HPT.T_Unit)
            }
          nodeSetOne = mkNodeSet [(cOne,  [])]
          nodeSetTwo = mkNodeSet [(cTwo,  [])]

          unspecLocExpectedRegisters = Map.fromList
            [ ("k0", tySetFromTypes [HPT.T_Int64])
            , ("p0", tySetFromTypes [HPT.T_Location 0, HPT.T_Location 1])
            , ("n0", tySetFromNodes (nodeSetOne <> nodeSetTwo))
            , ("one", tySetFromNodes nodeSetOne)
            , ("two", tySetFromNodes nodeSetTwo)

            , ("_1", tySetFromTypes [HPT.T_Int64])
            , ("_2", tySetFromTypes [HPT.T_Int64])
            , ("_3", tySetFromNodes nodeSetOne)
            , ("_4", tySetFromNodes nodeSetTwo)
            ]
      (calcHPTResult exp) `shouldBe` expected

calcHPTResult :: Exp -> HPTResult
calcHPTResult prog
  | (hptProgram, hptMapping) <- codeGen prog
  , computer <- _airComp . evalAbstractProgram $ hptProgram
  = toHPTResult hptMapping computer
