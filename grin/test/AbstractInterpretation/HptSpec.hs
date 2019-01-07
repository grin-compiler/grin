{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module AbstractInterpretation.HptSpec where

import Test.Hspec
import Test.Util
import Grin.Grin
import Grin.TH
import Grin.Lint
import Grin.TypeEnv as TypeEnv
import Grin.TypeCheck
import Grin.PrimOpsPrelude

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import AbstractInterpretation.HeapPointsTo.Result as HPT
import AbstractInterpretation.Reduce (evalAbstractProgram, _airComp)
import AbstractInterpretation.HeapPointsTo.CodeGen (codeGen)

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

  describe "case" $ do
    it "default bug01" $ do
      let code = withPrimPrelude [prog|
        grinMain =
          (CInt x) <- test 1 2
          y <- pure x
          _prim_int_print y

        test a b =
          c <- _prim_int_add a b
          case c of
                0 -> pure (CInt 100)
                1 ->
                  e0 <- pure c
                  pure (CInt e0)
                #default ->
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
      mkTySet = TypeSet mempty . mkNodeSet
      tySetFromTypes = flip TypeSet mempty . Set.fromList
      mkSimpleMain t = (tySetFromTypes [t], mempty)

  describe "HPT Result" $ do
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
            , HPT._function = Map.singleton "grinMain" (mkSimpleMain HPT.T_Int64)
            }
          nodeSetN0 = mkNodeSet [(cCons, [[HPT.T_Int64], [HPT.T_Location 0, HPT.T_Location 1]])]
          undefinedExpectedHeap = V.fromList
            [ mkNodeSet [(cNil, [])]
            , mkNodeSet [(cCons, [[HPT.T_Int64], [HPT.T_Location 0]])]
            , nodeSetN0
            ]
          undefinedExpectedRegisters = Map.fromList
            [ ("p0", loc 0)
            , ("p1", loc 1)
            , ("p2", loc 2)
            , ("x0", tySetFromTypes [HPT.T_Int64])
            , ("n0", TypeSet mempty nodeSetN0)
            , ("n1", typeN1)
            , ("n2", typeN2)
            ]
          typeN1 = mkTySet [ (cCons, [[HPT.T_Int64], [HPT.T_Location 2]]) , (cNil, []) ]
          typeN2 = mkTySet [ (cCons, [[HPT.T_Int64], [HPT.T_Location 0]]) ]

      (calcHPTResult exp) `shouldBe` expected

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
            { HPT._memory   = V.fromList [ nodeSetN0 ]
            , HPT._register = unspecLocExpectedRegisters
            , HPT._function = Map.singleton "grinMain" (mkSimpleMain HPT.T_Unit)
            }
          nodeSetN0 = mkNodeSet [(cInt,  [[HPT.T_Int64]])]
          nodeSetN1 = mkNodeSet [(cNode, [[HPT.T_UnspecifiedLocation]])]

          unspecLocExpectedRegisters = Map.fromList
            [ ("p0", tySetFromTypes [HPT.T_Location 0, HPT.T_UnspecifiedLocation])
            , ("p1", tySetFromTypes [HPT.T_UnspecifiedLocation])
            , ("n0", TypeSet mempty nodeSetN0)
            , ("n1", TypeSet mempty nodeSetN1)
            , ("x0", tySetFromTypes [])
            ]
      (calcHPTResult exp) `shouldBe` expected

calcHPTResult :: Exp -> HPTResult
calcHPTResult prog
  | (hptProgram, hptMapping) <- codeGen prog
  , computer <- _airComp . evalAbstractProgram $ hptProgram
  = toHPTResult hptMapping computer
