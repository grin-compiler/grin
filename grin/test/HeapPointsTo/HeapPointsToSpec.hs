{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module HeapPointsTo.HeapPointsToSpec where

import System.FilePath

import Grin.Grin
import Grin.TH
-- import Grin.TypeEnv hiding (NodeSet(..), T_Int64, T_Location, T_UnspecifiedLocation, T_Unit)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Test.IO
import Test.Hspec
import Test.Util
import Test.Assertions

import AbstractInterpretation.Reduce (evalDataFlowInfo, _airComp)
import AbstractInterpretation.HeapPointsTo (codeGen)
import AbstractInterpretation.HPTResult as HPT


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = describe "Heap Points To" $ do
  let loc = tySetFromTypes . pure . T_Location
      mkNode = V.fromList . map S.fromList
      mkNodeSet = NodeSet . M.fromList . map (\(t,v) -> (t,mkNode v))
      mkTySet = TypeSet mempty . mkNodeSet
      tySetFromTypes = flip TypeSet mempty . S.fromList
      mkSimpleMain t = (tySetFromTypes [t], mempty)

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
          , HPT._function = M.singleton "grinMain" (mkSimpleMain T_Int64)
          }
        nodeSetN0 = mkNodeSet [(cCons, [[T_Int64], [T_Location 0, T_Location 1]])]
        undefinedExpectedHeap = V.fromList
          [ mkNodeSet [(cNil, [])]
          , mkNodeSet [(cCons, [[T_Int64], [T_Location 0]])]
          , nodeSetN0
          ]
        undefinedExpectedRegisters = M.fromList
          [ ("p0", loc 0)
          , ("p1", loc 1)
          , ("p2", loc 2)
          , ("x0", tySetFromTypes [T_Int64])
          , ("n0", TypeSet mempty nodeSetN0)
          , ("n1", typeN1)
          , ("n2", typeN2)
          ]
        typeN1 = mkTySet [ (cCons, [[T_Int64], [T_Location 2]]) , (cNil, []) ]
        typeN2 = mkTySet [ (cCons, [[T_Int64], [T_Location 0]]) ]

    (calcHPTResult exp) `sameAs` expected

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
          , HPT._function = M.singleton "grinMain" (mkSimpleMain T_Unit)
          }
        nodeSetN0 = mkNodeSet [(cInt,  [[T_Int64]])]
        nodeSetN1 = mkNodeSet [(cNode, [[T_UnspecifiedLocation]])]

        unspecLocExpectedRegisters = M.fromList
          [ ("p0", tySetFromTypes [T_Location 0, T_UnspecifiedLocation])
          , ("p1", tySetFromTypes [T_UnspecifiedLocation])
          , ("n0", TypeSet mempty nodeSetN0)
          , ("n1", TypeSet mempty nodeSetN1)
          , ("x0", tySetFromTypes [])
          ]
    (calcHPTResult exp) `sameAs` expected

calcHPTResult :: Exp -> HPTResult
calcHPTResult prog
  | Right hptProgram <- codeGen prog
  , computer <- _airComp . evalDataFlowInfo $ hptProgram
  = toHPTResult hptProgram computer
