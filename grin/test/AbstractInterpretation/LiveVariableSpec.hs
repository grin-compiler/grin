{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes #-}
module AbstractInterpretation.LiveVariableSpec where

import Data.Map    (Map)
import Data.Vector (Vector)
import qualified Data.Map    as M
import qualified Data.Vector as V

import Grin.TH
import Grin.Grin
import Grin.PrimOpsPrelude

import Test.Hspec
import Test.Assertions
import Test.Util

import AbstractInterpretation.Reduce (AbstractInterpretationResult(..),evalAbstractProgram)
import AbstractInterpretation.LiveVariable.CodeGen hiding (live)
import AbstractInterpretation.LiveVariable.Result

-- TODO: Add tests includeing side effects

runTests :: IO ()
runTests = hspec spec

calcLiveness :: Exp -> LVAResult
calcLiveness prog
  | (lvaProgram, lvaMapping) <- codeGen Nothing prog
  , computer <- _airComp . evalAbstractProgram $ lvaProgram
  = toLVAResult lvaMapping computer

spec :: Spec
spec = describe "Live Variable Analysis" $ do

  it "case_anonymous" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 5
            case (CBool 0) of
              (CBool c0) -> pure (CBool c0)
              (CWord c1) -> pure (CNode c1)
              #default   -> pure (CWord a0)
        |]
    let caseAnonymousExpected = LVAResult
          { _memory   = []
          , _register = [ ("a0", deadVal), ("c0", liveVal), ("c1", deadVal) ]
          , _function = [ ("grinMain", fun (nodeSet [ (cBool, [live]) ], [])) ]
          }
    (calcLiveness exp) `sameAs` caseAnonymousExpected

  it "case_min_lit" $ do
    let exp = [prog|
          grinMain =
            a <- pure 0
            b <- pure 0
            c <- pure 0
            d <- pure 0
            e <- pure 0
            case a of
              0 -> pure b
              1 -> pure c
              2 -> pure d
        |]
    let caseMinLitExpected = LVAResult
          { _memory   = []
          , _register =
              [ ("a", liveVal)
              , ("b", liveVal)
              , ("c", liveVal)
              , ("d", liveVal)
              , ("e", deadVal)
              ]
          , _function = mkFunctionLivenessMap []
          }
    (calcLiveness exp) `sameAs` caseMinLitExpected

  it "case_min_nodes" $ do
    let exp = [prog|
          grinMain =
            n0 <- pure (CBool 0)
            n1 <- case n0 of
              (CBool c0) -> pure (CNode c0)
              (CWord c1) -> pure (CWord c1)
              #default   -> pure (CNope 5)
            (CNode b0) <- pure n1
            pure b0
        |]
    let caseMinNodesExpected = LVAResult
          { _memory   = []
          , _register =
              [ ("n0", livenessN0)
              , ("n1", livenessN1)
              , ("c0", liveVal)
              , ("c1", deadVal)
              , ("b0", liveVal)
              ]
          , _function = mkFunctionLivenessMap []
          }
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cNode, [live]) ]
    (calcLiveness exp) `sameAs` caseMinNodesExpected

  it "case_nested" $ do
    let exp = [prog|
          grinMain =
            n0 <- f 0
            b0 <- pure 0
            b1 <- pure 0
            case n0 of
              (CBool c0) -> case n0 of
                              (CBool c1) -> pure (CBool c1)
                              (CWord c2) -> pure (CWord b0)
                              (CNope c3) -> pure (CNope c3)
              (CWord c4) -> case n0 of
                              (CBool c5) -> pure (CBool b1)
                              (CWord c6) -> pure (CWord c4)
                              (CNope c7) -> pure (CCNope c7)
          f x =
            case x of
              0 -> pure (CBool 0)
              1 -> pure (CWord 0)
        |]
    let caseNestedExpected = LVAResult
          { _memory   = []
          , _register = caseNestedExpectedRegisters
          , _function = caseNestedExpectedFunctions
          }
        livenessFRet = nodeSet [ (cBool, [live]), (cWord, [live]) ]
        livenessMainRet = nodeSet [ (cBool, [live]), (cWord, [live]) ]
        caseNestedExpectedRegisters =
          [ ("n0", livenessFRet)
          , ("c0", deadVal)
          , ("c1", liveVal)
          , ("c2", deadVal)
          , ("c3", deadVal)
          , ("c4", liveVal)
          , ("c5", deadVal)
          , ("c6", deadVal)
          , ("c7", deadVal)
          , ("b0", deadVal)
          , ("b1", deadVal)
          , ("x",  liveVal)
          ]
        caseNestedExpectedFunctions =
          [ ("f", fun (livenessFRet, [liveVal]))
          , ("grinMain", fun (livenessMainRet,[]))
          ]
    (calcLiveness exp) `sameAs` caseNestedExpected

  it "case_restricted" $ do
    let exp = [prog|
          grinMain =
            n0 <- f 0
            case n0 of
              (CInt  c0) -> (CInt  b0) <- pure n0
                            pure b0
              (CBool c1) -> pure c1
              #default   -> (CWord b1) <- pure n0
                            pure b1

          f x =
            case x of
              0 -> pure (CInt  0)
              1 -> pure (CWord 0)
        |]
        caseRestrictedExpected = LVAResult
          { _memory   = []
          , _register = caseRestrictedExpectedRegisters
          , _function = caseRestrictedExpectedFunctions
          }
        caseRestrictedExpectedRegisters =
          [ ("n0", livenessFRet)
          , ("c0", deadVal)
          , ("c1", deadVal)
          , ("b0", liveVal)
          , ("b1", liveVal)
          , ("x",  liveVal)
          ]
        caseRestrictedExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessFRet, [liveVal])) ]
        livenessFRet = nodeSet [ (cInt,  [live]), (cWord, [live]) ]
    (calcLiveness exp) `sameAs` caseRestrictedExpected

  it "case_restricted_nodes" $ do
    let exp = [prog|
          grinMain =
            n0 <- f 0
            n4 <- case n0 of
              (CInt  c0) -> (CInt  b0) <- pure n0
                            n1 <- f b0
                            pure n1
              (CBool c1) -> n2 <- f c1
                            pure n2
              #default   -> (CWord b1) <- pure n0
                            n3 <- f b1
                            pure n3
            pure n4

          f x =
            case x of
              0 -> pure (CInt  0)
              1 -> pure (CBool 0)
              2 -> pure (CWord 0)
        |]
        caseRestrictedNodesExpected = LVAResult
          { _memory   = []
          , _register = caseRestrictedNodesExpectedRegisters
          , _function = caseRestrictedNodesExpectedFunctions
          }
        caseRestrictedNodesExpectedRegisters =
          [ ("n0", livenessFRet)
          , ("n1", livenessFRet)
          , ("n2", livenessFRet)
          , ("n3", livenessFRet)
          , ("n4", livenessFRet)
          , ("c0", deadVal)
          , ("c1", liveVal)
          , ("b0", liveVal)
          , ("b1", liveVal)
          , ("x",  liveVal)
          ]
        caseRestrictedNodesExpectedFunctions =
          [ ("f",        fun (livenessFRet, [liveVal]))
          , ("grinMain", fun (livenessFRet, []))
          ]
        livenessFRet = nodeSet [ (cInt,  [live]), (cBool, [live]), (cWord, [live]) ]
    (calcLiveness exp) `sameAs` caseRestrictedNodesExpected

  it "dead_tags" $ do
    let exp = [prog|
          grinMain =
            n0 <- pure (CNil)
            p0 <- store n0
            n1 <- pure (CCons 0 p0)
            p1 <- store n1

            update p0 (CInt 5)
            update p1 (CInt 5)

            n4 <- case 0 of
              0 ->
                n2 <- fetch p0
                pure n2
              1 ->
                n3 <- fetch p1
                pure n3

            case n4 of
              (CWord c0) -> pure 0
              #default   -> pure 1
        |]
        deadTagsExpected = LVAResult
          { _memory   = [ livenessN2, livenessN3 ]
          , _register = deadTagsExpectedRegisters
          , _function = mkFunctionLivenessMap []
          }
        deadTagsExpectedRegisters =
          [ ("p0", deadVal)
          , ("p1", deadVal)
          , ("n0", livenessN0)
          , ("n1", livenessN1)
          , ("n2", livenessN2)
          , ("n3", livenessN3)
          , ("n4", livenessN4)
          , ("c0", deadVal)
          ]
        livenessN0 = deadNodeSet [ (cNil, 0) ]
        livenessN1 = deadNodeSet [ (cCons, 2) ]
        livenessN2 = deadNodeSet [ (cNil, 0),  (cInt, 1) ]
        livenessN3 = deadNodeSet [ (cCons, 2), (cInt, 1) ]
        livenessN4 = deadNodeSet [ (cNil, 0), (cCons, 2),  (cInt, 1) ]
    (calcLiveness exp) `sameAs` deadTagsExpected

  it "fields" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 0
            a1 <- pure 1
            n0 <- pure (CNode a0 a1)
            case n0 of
              (CNode c0 c1) -> n1 <- pure (CNode c0 c1)
                               f n1

          f x =
            case x of
              (CNode c2 c3) -> pure c3
        |]
    let fieldsExpected = LVAResult
          { _memory   = []
          , _register = fieldsExpectedRegisters
          , _function = fieldsExpectedFunctions
          }
        fieldsExpectedRegisters =
          [ ("a0", deadVal)
          , ("a1", liveVal)
          , ("n0", livenessX)
          , ("c0", deadVal)
          , ("c1", liveVal)
          , ("c2", deadVal)
          , ("c3", liveVal)
          , ("n1", livenessX)
          , ("x",  livenessX)
          ]
        fieldsExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (liveVal, [livenessX])) ]
        livenessX = nodeSet [ (cNode, [dead, live]) ]
    (calcLiveness exp) `sameAs` fieldsExpected

  it "function_call_1" $ do
    let exp = [prog|
          grinMain =
            n <- pure (CFoo 0)
            y <- f n
            pure y

          f x =
            case x of
              (CFoo c0) -> pure c0
              (CBar c1) -> pure 5
        |]
    let functionCall1Expected = LVAResult
          { _memory   = []
          , _register = functionCall1ExpectedRegisters
          , _function = functionCall1ExpectedFunctions
          }
        functionCall1ExpectedRegisters =
          [ ("n",  livenessN)
          , ("y",  liveVal)
          , ("c0", liveVal)
          , ("c1", deadVal)
          , ("x",  livenessX)
          ]
        livenessN = nodeSet [ (cFoo, [live]) ]
        livenessX = nodeSet [ (cFoo, [live]) ]
        functionCall1ExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (liveVal, [livenessX])) ]
    (calcLiveness exp) `sameAs` functionCall1Expected

  it "function_call_2" $ do
    let exp = [prog|
          grinMain =
            (CTwo a1 b1) <- f
            n <- pure (COne a1)
            pure n

          f =
            a0 <- pure 0
            b0 <- pure 0
            pure (CTwo a0 b0)
        |]
    let functionCall2Expected = LVAResult
          { _memory   = []
          , _register = functionCall2ExpectedRegisters
          , _function = functionCall2ExpectedFunctions
          }
        functionCall2ExpectedRegisters =
          [ ("a0", liveVal)
          , ("a1", liveVal)
          , ("b0", deadVal)
          , ("b1", deadVal)
          , ("n",  livenessN)
          ]
        livenessN = nodeSet [ (cOne, [live]) ]
        functionCall2ExpectedFunctions =
          [ ("f",        fun (nodeSet [ (cTwo, [live, dead]) ], []))
          , ("grinMain", fun (nodeSet [ (cOne, [live]) ], []))
          ]
    (calcLiveness exp) `sameAs` functionCall2Expected

  it "heap_case_min" $ do
    let exp = [prog|
          grinMain =
            p0 <- case (CBool 0) of
              (CBool c1) -> store (CBool c1)
            (CBool a1) <- fetch p0
            pure a1
        |]
    let heapCaseMinExpected = LVAResult
          { _memory   = [ livenessLoc1 ]
          , _register = [ ("p0", liveVal), ("c1", liveVal), ("a1", liveVal) ]
          , _function = mkFunctionLivenessMap []
          }
        livenessLoc1 = nodeSet [ (cBool, [live]) ]
    (calcLiveness exp) `sameAs` heapCaseMinExpected

  it "heap_case" $ do
    let exp = [prog|
          grinMain =
            n0 <- f 0
            p0 <- case n0 of
              (CWord c0) -> store (CWordH c0)
              (CBool c1) -> store (CBoolH c1)
              (CNope c2) -> store (CNopeH c2)
            n1 <- fetch p0
            case n1 of
              (CWordH c3) -> pure 5
              (CBoolH c4) -> pure c4
              (CNopeH c5) -> pure c5

          f x =
            case x of
              0 -> pure (CBool 0)
              1 -> pure (CWord 0)
        |]
    let heapCaseExpected = LVAResult
          { _memory   = heapCaseExpectedHeap
          , _register = heapCaseExpectedRegisters
          , _function = heapCaseExpectedFunctions
          }
        heapCaseExpectedHeap =
          [ nodeSet [ (cWordH, [dead]) ]
          , nodeSet [ (cBoolH, [live]) ]
          , deadLoc -- the heap location is generated, but the instructions aren't executed
          ]
        heapCaseExpectedRegisters =
          [ ("n0", livenessFRet)
          , ("p0", liveVal)
          , ("c0", deadVal)
          , ("c1", liveVal)
          , ("c2", deadVal) -- the register is generated, but the instructions aren't executed
          , ("c3", deadVal)
          , ("c4", liveVal)
          , ("c5", deadVal) -- the register is generated, but the instructions aren't executed
          , ("n1", livenessN1)
          , ("x",  liveVal)
          ]
        livenessN1 = nodeSet [ (cBoolH, [live]), (cWordH, [dead]) ]
        heapCaseExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessFRet,[liveVal])) ]
        livenessLoc1 = nodeSet [ (cBoolH, [live]), (cWordH, [dead]) ]
        livenessFRet = nodeSet [ (cBool, [live]), (cWord, [dead]) ]
    (calcLiveness exp) `sameAs` heapCaseExpected

  it "heap_indirect_simple" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 5
            n0 <- pure (CNil)
            p0 <- store n0
            n1 <- pure (CCons a0 p0)
            r <- case n1 of
              (CNil) ->
                pure (CNil)
              (CCons x xs) ->
                xs' <- fetch xs
                pure xs'
            pure r
        |]
    let heapIndirectSimpleExpected = LVAResult
          { _memory   = [ nodeSet [ (cNil, []) ] ]
          , _register = heapIndirectSimpleExpectedRegisters
          , _function = [ ("grinMain", fun (livenessMainRet,[])) ]
          }
        heapIndirectSimpleExpectedRegisters =
          [ ("a0", deadVal)
          , ("n0", livenessN0)
          , ("p0", liveVal)
          , ("n1", livenessN1)
          , ("r", livenessN0)
          , ("x", deadVal)
          , ("xs", liveVal)
          , ("xs'", livenessN0)
          ]
        livenessN0 = nodeSet [ (cNil, []) ]
        livenessN1 = nodeSet [ (cCons, [dead,live]) ]
        livenessMainRet = nodeSet [ (cNil, []) ]
    (calcLiveness exp) `sameAs` heapIndirectSimpleExpected

  it "heap_simple" $ do
    let exp = [prog|
          grinMain =
            n <- pure (CTwo 0 1)
            p <- store n
            x <- fetch p
            (CTwo a b) <- pure x
            pure a
        |]
    let heapSimpleExpected = LVAResult
          { _memory   = heapSimpleExpectedHeap
          , _register = heapSimpleExpectedRegisters
          , _function = mkFunctionLivenessMap []
          }
        heapSimpleExpectedHeap = [ nodeSet $ [ (cTwo, [live, dead]) ] ]
        heapSimpleExpectedRegisters =
          [ ("n", livenessN)
          , ("p", liveVal)
          , ("x", livenessX)
          , ("a", liveVal)
          , ("b", deadVal)
          ]
        livenessN = nodeSet $ [ (cTwo, [live, dead]) ]
        livenessX = livenessN
    (calcLiveness exp) `sameAs` heapSimpleExpected

  it "heap_update_complex" $ do
    let exp = [prog|
          grinMain =
            n0 <- pure (CBool 0)
            n1 <- pure (CWord 0)
            p0 <- case 0 of
                    0 -> store n0
                    1 -> store n1
            n2 <- pure (CNode 0)
            update p0 n2
            n3 <- fetch p0
            case n3 of
              (CBool c0) -> pure c0
              (CWord c1) -> pure 0
              (CNode c2) -> pure c2
              (CNope c3) -> pure c3
        |]
    let heapUpdateComplexExpected = LVAResult
          { _memory   = heapUpdateComplexExpectedHeap
          , _register = heapUpdateComplexExpectedRegisters
          , _function = mkFunctionLivenessMap []
          }
        heapUpdateComplexExpectedHeap =
          [ nodeSet [ (cBool, [live]), (cNode, [live]) ]
          , nodeSet [ (cWord, [dead]), (cNode, [live]) ]
          ]
        heapUpdateComplexExpectedRegisters =
          [ ("n0", livenessN0)
          , ("n1", livenessN1)
          , ("n2", livenessN2)
          , ("n3", livenessN3)
          , ("p0", liveVal)
          , ("c0", liveVal)
          , ("c1", deadVal)
          , ("c2", liveVal)
          , ("c3", deadVal)
          ]
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cWord, [dead]) ]
        livenessN2 = nodeSet [ (cNode, [live]) ]
        livenessN3 = nodeSet [ (cBool, [live])
                             , (cWord, [dead])
                             , (cNode, [live])
                             ]
    (calcLiveness exp) `sameAs` heapUpdateComplexExpected

  it "heap_update_fun_call" $ do
    let exp = [prog|
          grinMain =
            n0 <- pure (CBool 0)
            n1 <- pure (CWord 0)
            p0 <- f 0 n0 n1
            n2 <- pure (CNode 0)
            q0 <- g p0 n2
            n3 <- fetch p0
            case n3 of
              (CBool c0) -> pure c0
              (CWord c1) -> pure 0
              (CNode c2) -> pure c2
              (CNope c3) -> pure c3

          f x y z =
            case 0 of
              0 -> store y
              1 -> store z

          g u v =
            update u v
        |]
    let heapUpdateFunCallExpected = LVAResult
          { _memory   = heapUpdateFunCallExpectedHeap
          , _register = heapUpdateFunCallExpectedRegisters
          , _function = heapUpdateFunCallExpectedFunctions
          }
        heapUpdateFunCallExpectedHeap =
          [ nodeSet [ (cBool, [live]), (cNode, [live]) ]
          , nodeSet [ (cWord, [dead]), (cNode, [live]) ]
          ]
        heapUpdateFunCallExpectedRegisters =
          [ ("n0", livenessN0)
          , ("n1", livenessN1)
          , ("n2", livenessN2)
          , ("n3", livenessN3)
          , ("p0", liveVal)
          , ("c0", liveVal)
          , ("c1", deadVal)
          , ("c2", liveVal)
          , ("c3", deadVal)
          , ("q0", deadVal)
          , ("x",  deadVal)
          , ("y",  livenessN0)
          , ("z",  livenessN1)
          , ("u",  liveVal)
          , ("v",  livenessN2)
          ]
        livenessN3 = nodeSet [ (cBool, [live])
                             , (cWord, [dead])
                             , (cNode, [live])
                             ]
        heapUpdateFunCallExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (liveVal, [deadVal, livenessN0, livenessN1]))
          , ("g", fun (deadVal, [liveVal, livenessN2]))
          ]
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cWord, [dead]) ]
        livenessN2 = nodeSet [ (cNode, [live]) ]
    (calcLiveness exp) `sameAs` heapUpdateFunCallExpected

  it "heap_update_local" $ do
    let exp = [prog|
          grinMain =
            n0 <- pure (CBool 0)
            n1 <- pure (CWord 0)
            p0 <- store n0
            update p0 n1
            n2 <- fetch p0
            case n2 of
              (CBool c0) -> pure c0
              (CWord c1) -> pure 5
              (CNope c2) -> pure c2
        |]
    let heapUpdateLocalExpected = LVAResult
          { _memory   = heapUpdateLocalExpectedHeap
          , _register = heapUpdateLocalExpectedRegisters
          , _function = mkFunctionLivenessMap []
          }
        heapUpdateLocalExpectedHeap =
          [ nodeSet [ (cBool, [live]), (cWord, [dead]) ] ]
        heapUpdateLocalExpectedRegisters =
          [ ("n0", livenessN0)
          , ("n1", livenessN1)
          , ("n2", livenessN2)
          , ("p0", liveVal)
          , ("c0", liveVal)
          , ("c1", deadVal)
          , ("c2", deadVal)
          ]
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cWord, [dead]) ]
        livenessN2 = nodeSet [ (cBool, [live]), (cWord, [dead]) ]
    (calcLiveness exp) `sameAs` heapUpdateLocalExpected

  it "lit_pat" $ do
    let exp = [prog|
          grinMain =
            y <- pure 0
            5 <- f y
            pure 0

          f x = pure x
        |]
    let litPatExpected = LVAResult
          { _memory   = []
          , _register = [ ("y", liveVal), ("x", liveVal) ]
          , _function = litPatExpectedFunctions
          }
        litPatExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (liveVal, [liveVal])) ]
    (calcLiveness exp) `sameAs` litPatExpected

  it "var_pat" $ do
    let exp = [prog|
          grinMain =
            y <- pure 0
            z <- f y
            pure y

          f x = pure x
        |]
    let varPatExpected = LVAResult
          { _memory   = []
          , _register = [ ("z", deadVal), ("y", liveVal), ("x", deadVal) ]
          , _function = varPatExpectedFunctions
          }
        varPatExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (deadVal, [deadVal])) ]
    (calcLiveness exp) `sameAs` varPatExpected

  it "node_pat" $ do
    let exp = [prog|
          grinMain =
            y <- pure (CInt 5)
            (CInt n) <- f y
            pure y

          f x = pure x
        |]
    let nodePatExpected = LVAResult
          { _memory   = []
          , _register = [ ("n", deadVal), ("y", livenessY), ("x", livenessFRet) ]
          , _function = nodePatExpectedFunctions
          }
        nodePatExpectedFunctions =
          [ ("f",        fun (livenessFRet, [livenessFArg]))
          , ("grinMain", fun (livenessY, []))
          ]

        livenessY    = nodeSet [ (cInt, [live]) ]
        livenessFRet = nodeSet [ (cInt, [dead]) ]
        livenessFArg = livenessFRet

    (calcLiveness exp) `sameAs` nodePatExpected

  it "main_node_ret" $ do
    let exp = [prog|
          grinMain =
            a <- pure 0
            b <- pure 0
            n <- pure (CTwo a b)
            pure n
        |]
    let mainNodeRetExpected = LVAResult
          { _memory   = []
          , _register = mainNodeRetExpectedRegisters
          , _function = [ ("grinMain", fun (livenessN, [])) ]
          }
        mainNodeRetExpectedRegisters =
          [ ("a", liveVal)
          , ("b", liveVal)
          , ("n", livenessN)
          ]
        livenessN = nodeSet [ (cTwo, [live, live]) ]
    (calcLiveness exp) `sameAs` mainNodeRetExpected

  it "nodes_simple" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 0
            a1 <- pure 1
            a2 <- pure 2
            n0 <- f a0 a1 a2
            case n0 of
              (CInt  c0) -> pure c0
              (CBool c1) -> pure 5

          f x y z =
            case x of
              0 -> pure (CInt  y)
              1 -> pure (CBool z)
        |]
    let nodesSimpleExpected = LVAResult
          { _memory   = []
          , _register = nodesSimpleExpectedRegisters
          , _function = nodesSimpleExpectedFunctions
          }
        nodesSimpleExpectedRegisters =
          [ ("a0", liveVal)
          , ("a1", liveVal)
          , ("a2", deadVal)
          , ("n0", livenessN0)
          , ("c0", liveVal)
          , ("c1", deadVal)
          , ("x",  liveVal)
          , ("y",  liveVal)
          , ("z",  deadVal)
          ]
        livenessN = nodeSet [ (cFoo, [live]) ]
        nodesSimpleExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessN0, [liveVal, liveVal, deadVal])) ]
        livenessN0 = nodeSet [ (cInt,  [live]), (cBool, [dead]) ]
    (calcLiveness exp) `sameAs` nodesSimpleExpected

  it "nodes_tricky" $ do
    pendingWith "illegal grin code: every node tag must have a single arity for the whole program"
    let exp = [prog|
          grinMain =
            n0 <- f 0
            case n0 of
              (COne c0 c1) -> pure 5
              (CTwo c2 c3) -> pure c2

          f x =
            case x of
              0 -> pure (COne x)
              1 -> pure (CTwo 0 x)
        |]
    let nodesTrickyExpected = LVAResult
          { _memory   = []
          , _register = nodesTrickyExpectedRegisters
          , _function = nodesTrickyExpectedFunctions
          }
        nodesTrickyExpectedRegisters =
          [ ("n0", livenessN0)
          , ("c0", deadVal)
          , ("c1", deadVal)
          , ("c2", liveVal)
          , ("c3", deadVal)
          , ("x",  liveVal)
          ]
        nodesTrickyExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessN0, [liveVal])) ]
        livenessN0 = nodeSet [ (cOne, [dead]), (cTwo, [live, dead]) ]
    (calcLiveness exp) `sameAs` nodesTrickyExpected

  it "sum_opt" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            n13 <- sum 0 1 100000
            _prim_int_print n13

          sum n29 n30 n31 =
            b2 <- _prim_int_gt n30 n31
            if b2 then
              pure n29
            else
              n18 <- _prim_int_add n30 1
              n28 <- _prim_int_add n29 n30
              sum n28 n18 n31
        |]
    let sumOptExpected = LVAResult
          { _memory   = []
          , _register = sumOptExpectedRegisters
          , _function = sumOptExpectedFunctions
          }
        sumOptExpectedRegisters =
          [ ("n13", liveVal)
          , ("n29", liveVal)
          , ("n29", liveVal)
          , ("n30", liveVal)
          , ("n31", liveVal)
          , ("b2",  liveVal)
          , ("n18",  liveVal)
          , ("n28",  liveVal)
          ]
        sumOptExpectedFunctions = mkFunctionLivenessMap
          [ ("sum", fun (liveVal, [liveVal, liveVal, liveVal]))
          -- , ("_prim_int_add", fun (liveVal, [liveVal, liveVal]))
          -- , ("_prim_int_gt",  fun (liveVal, [liveVal, liveVal]))
          -- , ("_prim_int_print", fun (liveVal, [liveVal]))
          ]
    (calcLiveness exp) `sameAs` sumOptExpected

  it "undefined" $ do
    let exp = [prog|
          grinMain =
            p0 <- store (CNil)
            p1 <- store (CCons 0 p0)
            x0 <- pure (#undefined :: T_Int64)
            n0 <- pure (#undefined :: {CCons[T_Int64, #ptr]})
            n1 <- pure (CCons (#undefined :: T_Int64) p0)
            n2 <- pure (CCons (#undefined :: #ptr) p0)
            (CCons c0 c1) <- pure n2
            x1 <- fetch c0
            pure x1
        |]
    let undefinedExpected = LVAResult
          { _memory   = undefinedExpectedHeap
          , _register = undefinedExpectedRegisters
          , _function = mkFunctionLivenessMap []
          }
        undefinedExpectedHeap =
          [ deadNodeSet [ (cNil, 0) ]
          , cConsBothDead
          ]
        undefinedExpectedRegisters =
          [ ("p0", deadVal)
          , ("p1", deadVal)
          , ("x0", deadVal)
          , ("x1", liveVal)
          , ("n0", livenessN0)
          , ("n1", livenessN1)
          , ("n2", livenessN2)
          , ("c0", liveVal)
          , ("c1", deadVal)
          ]
        livenessN0 = cConsBothDead
        livenessN1 = cConsBothDead
        livenessN2 = nodeSet [ (cCons, [live, dead]) ]
        cConsBothDead = deadNodeSet [ (cCons, 2) ]
    (calcLiveness exp) `sameAs` undefinedExpected

  it "undefined_with_loc_info" $ do
    let exp = [prog|
          grinMain =
            p0 <- store (CNil)
            n0 <- pure (CCons (#undefined :: {0}) p0)
            (CCons c0 c1) <- pure n0
            x0 <- fetch c0
            pure x0
        |]
    let undefinedWithLocInfoExpected = LVAResult
          { _memory   = [ cNilLiveness ]
          , _register = undefinedWithLocInfoExpectedRegisters
          , _function = undefinedWithLocInfoExpectedFunctions
          }
        undefinedWithLocInfoExpectedRegisters =
          [ ("p0", deadVal)
          , ("x0", cNilLiveness)
          , ("n0", livenessN0)
          , ("c0", liveVal)
          , ("c1", deadVal)
          ]
        undefinedWithLocInfoExpectedFunctions =
          [ ("grinMain", fun (cNilLiveness, [])) ]
        livenessN0 = nodeSet [ (cCons, [live, dead]) ]
        cNilLiveness = nodeSet [ (cNil, []) ]
    (calcLiveness exp) `sameAs` undefinedWithLocInfoExpected

live :: Bool
live = True

dead :: Bool
dead = False

liveVal :: Liveness
liveVal = BasicVal True

deadVal :: Liveness
deadVal = BasicVal False

liveLoc :: Liveness
liveLoc = BasicVal True

deadLoc :: Liveness
deadLoc = BasicVal False

-- Nodes with tag liveness info
node' :: [Bool] -> Node
node' [] = error "Please provide tag liveness"
node' (tagLv:fieldsLv) = Node tagLv (V.fromList fieldsLv)

-- Node sets with tag liveness info
nodeSet' :: [(Tag,[Bool])] -> Liveness
nodeSet' = NodeSet . M.fromList . map (\(t,fs) -> (t, node' fs))

-- Node with dead tag and dead fields
deadNode :: Int -> Node
deadNode = Node <$> const False <*> flip V.replicate False

-- Node set with dead tags and dead fields
deadNodeSet :: [(Tag,Int)] -> Liveness
deadNodeSet = NodeSet . M.fromList . map (\(t,n) -> (t, deadNode n))

-- Node with live tag
node :: [Bool] -> Node
node = Node <$> const True <*> V.fromList

-- Node set with live tags
nodeSet :: [(Tag,[Bool])] -> Liveness
nodeSet = NodeSet . M.fromList . map (\(t,fs) -> (t, node fs))

fun :: (Liveness, [Liveness]) -> (Liveness, Vector Liveness)
fun = fmap V.fromList

mkFunctionLivenessMap :: [(Name, (Liveness, Vector Liveness))]
                      -> Map Name (Liveness, Vector Liveness)
mkFunctionLivenessMap = M.insert "grinMain" (fun (liveVal,[])) . M.fromList
