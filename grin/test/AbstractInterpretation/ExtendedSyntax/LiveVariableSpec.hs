{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes #-}
module AbstractInterpretation.ExtendedSyntax.LiveVariableSpec where

import Data.Map    (Map)
import Data.Vector (Vector)
import qualified Data.Map    as M
import qualified Data.Vector as V

import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.PrimOpsPrelude

import Test.Hspec
import Test.ExtendedSyntax.Assertions
import Test.ExtendedSyntax.Util

import AbstractInterpretation.ExtendedSyntax.Reduce (AbstractInterpretationResult(..),evalAbstractProgram)
import AbstractInterpretation.ExtendedSyntax.LiveVariable.CodeGen hiding (live)
import AbstractInterpretation.ExtendedSyntax.LiveVariable.Result

import AbstractInterpretation.ExtendedSyntax.EffectTrackingSpec (calcEffects)

{- NOTE: Hard to decipher error message for effects
   when the expected result is "no effect",
   but the actual result has no entry for the given variable.
   Nothing is displayed in the diff.
-}

{- NOTE: Variables with names like "z<i>" are introduced just for naming.
   They are not relevant to the result of the analysis.

   Variables with names like "_<i>" are introduced just for named bindings.
   They will never be used.

   Variables with names like "alt<i>" are introduced just for named case alternatives.
   They will never be used.
-}

runTests :: IO ()
runTests = hspec spec

calcLiveness :: Exp -> LVAResult
calcLiveness prog
  | (lvaProgram, lvaMapping) <- codeGen prog
  , computer <- _airComp . evalAbstractProgram $ lvaProgram
  = toLVAResult lvaMapping computer

spec :: Spec
spec = describe "Live Variable Analysis" $ do

  it "variable_alias" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            x <- pure 0
            y <- pure x
            _prim_int_print x
      |]
    let variableAliasExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = [ ("x", liveVal)
                          , ("y", deadVal)
                          ]
          , _functionLv = mkFunctionLivenessMap []
          }
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` variableAliasExpected

  it "as_pattern_with_node_1" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            x0 <- pure 0
            n0 <- pure (CInt x0)
            (CInt x1) @ _v <- pure n0
            _prim_int_print x1
      |]
    let variableAliasExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = [ ("x0", liveVal)
                          , ("x1", liveVal)
                          , ("n0", nodeSet [ (cInt, [live]) ])
                          , ("_v", deadNodeSet [ (cInt, 1) ])
                          ]
          , _functionLv = mkFunctionLivenessMap []
          }
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` variableAliasExpected

  it "as_pattern_with_node_2" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            x0 <- pure 0
            n0 <- pure (CInt x0)
            (CInt _v) @ n1 <- pure n0
            case n1 of
              (CInt c0) @ alt0 -> _prim_int_print c0
      |]
    let variableAliasExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = [ ("x0", liveVal)
                          , ("_v", deadVal)
                          , ("c0", liveVal)
                          , ("n0", nodeSet [ (cInt, [live]) ])
                          , ("n1", nodeSet [ (cInt, [live]) ])

                          , ("alt0", deadNodeSet [ (cInt, 1) ])
                          ]
          , _functionLv = mkFunctionLivenessMap []
          }
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` variableAliasExpected

  it "case_anonymous" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 5
            z0 <- pure 0
            n0 <- pure (CBool z0)
            case n0 of
              (CBool c0) @ alt0 -> pure (CBool c0)
              (CWord c1) @ alt1 -> pure (CNode c1)
              #default@alt2   -> pure (CWord a0)
        |]
    let caseAnonymousExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = [ ("a0", deadVal)
                          , ("c0", liveVal)
                          , ("c1", deadVal)
                          , ("n0", livenessN0)

                          , ("z0", liveVal)
                          , ("alt0", deadNodeSet [ (cBool, 1) ])
                          , ("alt1", deadVal)
                          , ("alt2", deadVal)
                          ]
          , _functionLv = [ ("grinMain", fun (livenessN0, [])) ]
          }
        livenessN0 = nodeSet [ (cBool, [live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` caseAnonymousExpected

  it "case_min_lit" $ do
    let exp = [prog|
          grinMain =
            a <- pure 0
            b <- pure 0
            c <- pure 0
            d <- pure 0
            e <- pure 0
            case a of
              0@alt0 -> pure b
              1@alt1 -> pure c
              2@alt2 -> pure d
        |]
    let caseMinLitExpected = emptyLVAResult
          { _memory     = []
          , _registerLv =
              [ ("a", liveVal)
              , ("b", liveVal)
              , ("c", liveVal)
              , ("d", liveVal)
              , ("e", deadVal)

              , ("alt0", deadVal)
              , ("alt1", deadVal)
              , ("alt2", deadVal)
              ]
          , _functionLv = mkFunctionLivenessMap []
          }
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` caseMinLitExpected

  -- QUESTION: is the liveness of _1 correct?
  it "case_min_nodes" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- pure (CBool z0)
            n1 <- case n0 of
              (CBool c0) @ alt0 -> pure (CNode c0)
              (CWord c1) @ alt1 -> pure (CWord c1)
              #default   @ alt2 -> pure (CNope)
            (CNode b0) @ _1 <- pure n1
            pure b0
        |]
    let caseMinNodesExpected = emptyLVAResult
          { _memory     = []
          , _registerLv =
              [ ("n0", livenessN0)
              , ("n1", livenessN1)
              , ("c0", liveVal)
              , ("c1", deadVal)
              , ("b0", liveVal)

              , ("z0", liveVal)
              , ("_1", nodeSet' [ (cNode, [dead, dead]) ])
              , ("alt0", deadNodeSet [ (cBool, 1) ])
              , ("alt1", deadVal)
              , ("alt2", deadVal)
              ]
          , _functionLv = mkFunctionLivenessMap []
          }
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cNode, [live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` caseMinNodesExpected

  it "case_nodes_named_alt" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- pure (CBool z0)
            n1 <- case n0 of
              (CBool c0) @ alt0 ->
                case alt0 of
                  (CBool c1) @ alt1 -> pure (CNode c1)
              (CWord c2) @ alt2 -> pure (CWord c2)
              #default @ alt3   -> pure (CNope)
            (CNode b0)@_1 <- pure n1
            pure b0
        |]
    let caseMinNodesExpected = emptyLVAResult
          { _memory     = []
          , _registerLv =
              [ ("n0", livenessN0)
              , ("n1", livenessN1)
              , ("c0", deadVal)
              , ("c1", liveVal)
              , ("c2", deadVal)
              , ("b0", liveVal)

              , ("z0", liveVal)
              , ("_1", nodeSet' [ (cNode, [dead, dead]) ])
              , ("alt0", nodeSet' [ (cBool, [live, live]) ])
              , ("alt1", deadNodeSet [ (cBool, 1) ])
              , ("alt2", deadVal)
              , ("alt3", deadVal)
              ]
          , _functionLv = mkFunctionLivenessMap []
          }
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cNode, [live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` caseMinNodesExpected

  it "case_nested" $ do
    let exp = [prog|
          grinMain =
            b0 <- pure 0
            b1 <- pure 0
            z0 <- pure 0
            n0 <- f z0
            case n0 of
              (CBool c0) @ alt0 ->
                case n0 of
                  (CBool c1) @ alt01 -> pure (CBool c1)
                  (CWord c2) @ alt02 -> pure (CWord b0)
                  (CNope c3) @ alt03 -> pure (CNope c3)
              (CWord c4) @ alt1 ->
                case n0 of
                  (CBool c5) @ alt11 -> pure (CBool b1)
                  (CWord c6) @ alt12 -> pure (CWord c4)
                  (CNope c7) @ alt13 -> pure (CNope c7)
          f x =
            z1 <- pure 0
            case x of
              0 @ alt2 -> pure (CBool z1)
              1 @ alt3 -> pure (CWord z1)
        |]
    let caseNestedExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = caseNestedExpectedRegisters
          , _functionLv = caseNestedExpectedFunctions
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

          , ("z0", liveVal)
          , ("z1", liveVal)
          , ("alt0",  deadNodeSet [ (cBool, 1) ])
          , ("alt01", deadNodeSet [ (cBool, 1) ])
          , ("alt02", deadVal)
          , ("alt03", deadVal)
          , ("alt1",  deadNodeSet [ (cWord, 1) ])
          , ("alt11", deadVal)
          , ("alt12", deadNodeSet [ (cWord, 1) ])
          , ("alt13", deadVal)
          , ("alt2", deadVal)
          , ("alt3", deadVal)
          ]
        caseNestedExpectedFunctions =
          [ ("f", fun (livenessFRet, [liveVal]))
          , ("grinMain", fun (livenessMainRet,[]))
          ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` caseNestedExpected

  it "case_restricted" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- f z0
            case n0 of
              (CInt  c0) @ alt0 ->
                (CInt b0) @ _1 <- pure n0
                pure b0
              (CBool c1) @ alt1 ->
                pure c1
              #default@alt2 ->
                (CWord b1) @ _2 <- pure n0
                pure b1

          f x =
            z1 <- pure 0
            case x of
              0@alt3 -> pure (CInt  z1)
              1@alt4 -> pure (CWord z1)
        |]
        caseRestrictedExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = caseRestrictedExpectedRegisters
          , _functionLv = caseRestrictedExpectedFunctions
          }
        caseRestrictedExpectedRegisters =
          [ ("n0", livenessFRet)
          , ("c0", deadVal)
          , ("c1", deadVal)
          , ("b0", liveVal)
          , ("b1", liveVal)
          , ("x",  liveVal)

          , ("z0", liveVal)
          , ("z1", liveVal)
          , ("_1", nodeSet' [ (cInt,  [dead, dead]) ])
          , ("_2", nodeSet' [ (cWord, [dead, dead]) ])
          , ("alt0", deadNodeSet [ (cInt, 1) ])
          , ("alt1", deadVal)
          , ("alt2", deadNodeSet [ (cWord, 1) ])
          , ("alt3", deadVal)
          , ("alt4", deadVal)
          ]
        caseRestrictedExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessFRet, [liveVal])) ]
        livenessFRet = nodeSet [ (cInt,  [live]), (cWord, [live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` caseRestrictedExpected

  it "case_restricted_nodes" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- f z0
            n4 <- case n0 of
              (CInt  c0) @ alt0 ->
                (CInt  b0) @ _1 <- pure n0
                n1 <- f b0
                pure n1
              (CBool c1) @ alt1 ->
                n2 <- f c1
                pure n2
              #default@alt2 ->
                (CWord b1) @ _2 <- pure n0
                n3 <- f b1
                pure n3
            pure n4

          f x =
            z1 <- pure 0
            case x of
              0@alt3 -> pure (CInt  z1)
              1@alt4 -> pure (CBool z1)
              2@alt5 -> pure (CWord z1)
        |]
        caseRestrictedNodesExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = caseRestrictedNodesExpectedRegisters
          , _functionLv = caseRestrictedNodesExpectedFunctions
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

          , ("z0", liveVal)
          , ("z1", liveVal)
          , ("_1", nodeSet' [ (cInt,  [dead, dead]) ])
          , ("_2", nodeSet' [ (cWord, [dead, dead]) ])
          , ("alt0", deadNodeSet [ (cInt,  1) ])
          , ("alt1", deadNodeSet [ (cBool, 1) ])
          , ("alt2", deadNodeSet [ (cWord, 1) ])
          , ("alt3", deadVal)
          , ("alt4", deadVal)
          , ("alt5", deadVal)
          ]
        caseRestrictedNodesExpectedFunctions =
          [ ("f",        fun (livenessFRet, [liveVal]))
          , ("grinMain", fun (livenessFRet, []))
          ]
        livenessFRet = nodeSet [ (cInt,  [live]), (cBool, [live]), (cWord, [live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` caseRestrictedNodesExpected

  {- NOTE: Here, we trust the code instead of relying on
     the gathered static information about the possible
     tags of a variable.

     Here statically `n4` could be any of `CNil`, `CCons`
     or `CInt` because LVA is flow-insensitive. However,
     we know that during every possible execution of the
     program `n4` could only be a `CInt`. So instead of
     marking all tags live, we only mark those that appear
     amongst the alternatives. Also, if there is a #default
     alternative, we mark all tags lives.

     Certain front ends could generate code like this, and
     it is their responsibility to make sure the pattern
     matches cannot fail.
  -}
  it "dead_tags" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0

            n0 <- pure (CNil)
            p0 <- store n0
            n1 <- pure (CCons z0 p0)
            p1 <- store n1

            z1 <- pure (CInt z0)
            _1 <- update p0 z1
            _2 <- update p1 z1

            n4 <- case z0 of
              0@alt0 ->
                n2 <- fetch p0
                pure n2
              1@alt1 ->
                n3 <- fetch p1
                pure n3

            case n4 of
              (CInt c0) @ alt2 -> pure z0
        |]
        deadTagsExpected = emptyLVAResult
          { _memory     = [ livenessN2, livenessN3 ]
          , _registerLv = deadTagsExpectedRegisters
          , _functionLv = mkFunctionLivenessMap []
          }
        deadTagsExpectedRegisters =
          [ ("p0", liveLoc)
          , ("p1", liveLoc)
          , ("n0", livenessN0)
          , ("n1", livenessN1)
          , ("n2", livenessN2)
          , ("n3", livenessN3)
          , ("n4", livenessN4)
          , ("c0", deadVal)

          , ("z0", liveVal)
          , ("z1", nodeSet [ (cInt, [dead]) ])
          , ("_1", deadLoc)
          , ("_2", deadLoc)
          , ("alt0", deadVal)
          , ("alt1", deadVal)
          , ("alt2", deadNodeSet [ (cInt, 1) ])
          ]
        livenessN0 = deadNodeSet [ (cNil, 0) ]
        livenessN1 = deadNodeSet [ (cCons, 2) ]
        livenessN2 = nodeSet' [ (cNil,  [dead])  ,            (cInt, [live, dead]) ]
        livenessN3 = nodeSet' [ (cCons, [dead, dead, dead]),  (cInt, [live, dead]) ]
        livenessN4 = nodeSet' [ (cNil, [dead]), (cCons, [dead, dead, dead]), (cInt, [live, dead]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` deadTagsExpected

  it "fields" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 0
            a1 <- pure 1
            n0 <- pure (CNode a0 a1)
            case n0 of
              (CNode c0 c1) @ alt0 ->
                n1 <- pure (CNode c0 c1)
                f n1

          f x =
            case x of
              (CNode c2 c3) @ alt1 -> pure c3
        |]
    let fieldsExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = fieldsExpectedRegisters
          , _functionLv = fieldsExpectedFunctions
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

          , ("alt0", deadNodeSet [ (cNode, 2) ])
          , ("alt1", deadNodeSet [ (cNode, 2) ])
          ]
        fieldsExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (liveVal, [livenessX])) ]
        livenessX = nodeSet [ (cNode, [dead, live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` fieldsExpected

  it "function_call_1" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n <- pure (CFoo z0)
            y <- f n
            pure y

          f x =
            case x of
              (CFoo c0) @ alt0 -> pure c0
              (CBar c1) @ alt1 -> pure z0
        |]
    let functionCall1Expected = emptyLVAResult
          { _memory     = []
          , _registerLv = functionCall1ExpectedRegisters
          , _functionLv = functionCall1ExpectedFunctions
          }
        functionCall1ExpectedRegisters =
          [ ("n",  livenessN)
          , ("y",  liveVal)
          , ("c0", liveVal)
          , ("c1", deadVal)
          , ("x",  livenessX)

          , ("z0", liveVal)
          , ("alt0", deadNodeSet [ (cFoo, 1) ])
          , ("alt1", deadVal)
          ]
        livenessN = nodeSet [ (cFoo, [live]) ]
        livenessX = nodeSet [ (cFoo, [live]) ]
        functionCall1ExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (liveVal, [livenessX])) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` functionCall1Expected

  it "function_call_2" $ do
    let exp = [prog|
          grinMain =
            (CTwo a1 b1) @ _1 <- f
            n <- pure (COne a1)
            pure n

          f =
            a0 <- pure 0
            b0 <- pure 0
            pure (CTwo a0 b0)
        |]
    let functionCall2Expected = emptyLVAResult
          { _memory     = []
          , _registerLv = functionCall2ExpectedRegisters
          , _functionLv = functionCall2ExpectedFunctions
          }
        functionCall2ExpectedRegisters =
          [ ("a0", liveVal)
          , ("a1", liveVal)
          , ("b0", deadVal)
          , ("b1", deadVal)
          , ("n",  livenessN)

          , ("_1", nodeSet' [(cTwo, [dead, dead, dead])] )
          ]
        livenessN = nodeSet [ (cOne, [live]) ]
        functionCall2ExpectedFunctions =
          [ ("f",        fun (nodeSet [ (cTwo, [live, dead]) ], []))
          , ("grinMain", fun (nodeSet [ (cOne, [live]) ], []))
          ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` functionCall2Expected

  it "heap_case_min" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            z1 <- pure (CBool z0)
            p0 <- case z1 of
              (CBool c1) @ alt0 ->
                z2 <- pure (CBool c1)
                store z2
            (CBool a1) @ _1 <- fetch p0
            pure a1
        |]
    let heapCaseMinExpected = emptyLVAResult
          { _memory     = [ livenessLoc1 ]
          , _registerLv = [ ("p0", liveVal)
                          , ("c1", liveVal)
                          , ("a1", liveVal)

                          , ("z0", liveVal)
                          , ("z1", nodeSet  [ (cBool, [live]) ])
                          , ("z2", nodeSet  [ (cBool, [live]) ])
                          , ("_1", nodeSet' [ (cBool, [dead, dead]) ])
                          , ("alt0", deadNodeSet [ (cBool, 1) ])
                          ]
          , _functionLv = mkFunctionLivenessMap []
          }
        livenessLoc1 = nodeSet [ (cBool, [live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapCaseMinExpected

  it "heap_case" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- f z0
            p0 <- case n0 of
              (CWord c0) @ alt0 ->
                z1 <- pure (CWordH c0)
                store z1
              (CBool c1) @ alt1 ->
                z2 <- pure (CBoolH c1)
                store z2
              (CNope c2) @ alt2 ->
                z3 <- pure (CNopeH c2)
                store z3
            n1 <- fetch p0
            case n1 of
              (CWordH c3) @ alt3 -> pure z0
              (CBoolH c4) @ alt4 -> pure c4
              (CNopeH c5) @ alt5 -> pure c5

          f x =
            z4 <- pure 0
            case x of
              0@alt6 -> pure (CBool z4)
              1@alt7 -> pure (CWord z4)
        |]
    let heapCaseExpected = emptyLVAResult
          { _memory     = heapCaseExpectedHeap
          , _registerLv = heapCaseExpectedRegisters
          , _functionLv = heapCaseExpectedFunctions
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

          , ("z0", liveVal)
          , ("z1", nodeSet  [ (cWordH, [dead]) ])
          , ("z2", nodeSet  [ (cBoolH, [live]) ])
          , ("z3", deadVal) -- deadVal because that case alternative is not analyzed
          , ("z4", liveVal)
          , ("alt0", deadNodeSet [ (cWord, 1) ])
          , ("alt1", deadNodeSet [ (cBool, 1) ])
          , ("alt2", deadVal)
          , ("alt3", deadNodeSet [ (cWordH, 1) ])
          , ("alt4", deadNodeSet [ (cBoolH, 1) ])
          , ("alt5", deadVal)
          , ("alt6", deadVal)
          , ("alt7", deadVal)
          ]
        livenessN1 = nodeSet [ (cBoolH, [live]), (cWordH, [dead]) ]
        heapCaseExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessFRet,[liveVal])) ]
        livenessLoc1 = nodeSet [ (cBoolH, [live]), (cWordH, [dead]) ]
        livenessFRet = nodeSet [ (cBool, [live]), (cWord, [dead]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapCaseExpected

  it "heap_case_default_pat" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            z1 <- pure 1
            z2 <- pure 2
            n0 <- pure (CNil z0)
            n1 <- pure (COne z1)
            n2 <- pure (CTwo z2)

            p0 <- store n0

            _1 <- update p0 n1
            _2 <- update p0 n2

            z3 <- pure 0
            n <- fetch p0
            x <- case n of
              (CNil c0) @ alt0 -> pure z3
              (COne c1) @ alt1 -> pure z3
              #default  @ alt2  -> pure z3

            pure x
        |]
    let heapCaseMinExpected = emptyLVAResult
          { _memory     = [ livenessLoc1 ]
          , _registerLv = [ ("n0", nodeSet' [ cNilLiveness ])
                          , ("n1", nodeSet' [ cOneLiveness ])
                          , ("n2", nodeSet' [ cTwoLiveness ])
                          , ("p0", liveLoc)
                          , ("n",  nodeSet' [ cNilLiveness, cOneLiveness, cTwoLiveness ] )
                          , ("c0", deadVal)
                          , ("c1", deadVal)
                          , ("x",  liveVal)

                          , ("z0",  deadVal)
                          , ("z1",  deadVal)
                          , ("z2",  deadVal)
                          , ("z3",  liveVal)
                          , ("_1",  deadVal)
                          , ("_2",  deadVal)
                          , ("alt0", deadNodeSet [ (cNil, 1) ])
                          , ("alt1", deadNodeSet [ (cOne, 1) ])
                          , ("alt2", deadNodeSet [ (cTwo, 1) ])
                          ]
          , _functionLv = mkFunctionLivenessMap []
          }
        cNilLiveness = (cNil, [live, dead])
        cOneLiveness = (cOne, [live, dead])
        cTwoLiveness = (cTwo, [live, dead])
        livenessLoc1 = nodeSet' [ cNilLiveness, cOneLiveness, cTwoLiveness ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapCaseMinExpected

  it "heap_indirect_simple" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 5
            n0 <- pure (CNil)
            p0 <- store n0
            n1 <- pure (CCons a0 p0)
            r <- case n1 of
              (CNil) @ alt0 ->
                pure (CNil)
              (CCons x xs) @ alt1 ->
                xs' <- fetch xs
                pure xs'
            pure r
        |]
    let heapIndirectSimpleExpected = emptyLVAResult
          { _memory     = [ nodeSet [ (cNil, []) ] ]
          , _registerLv = heapIndirectSimpleExpectedRegisters
          , _functionLv = [ ("grinMain", fun (livenessMainRet,[])) ]
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

          , ("alt0", deadVal)
          , ("alt1", deadNodeSet [ (cCons, 2) ])
          ]
        livenessN0 = nodeSet [ (cNil, []) ]
        livenessN1 = nodeSet [ (cCons, [dead,live]) ]
        livenessMainRet = nodeSet [ (cNil, []) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapIndirectSimpleExpected

  it "heap_simple" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            z1 <- pure 1
            n <- pure (CTwo z0 z1)
            p <- store n
            x <- fetch p
            (CTwo a b) @ _1 <- pure x
            pure a
        |]
    let heapSimpleExpected = emptyLVAResult
          { _memory     = heapSimpleExpectedHeap
          , _registerLv = heapSimpleExpectedRegisters
          , _functionLv = mkFunctionLivenessMap []
          }
        heapSimpleExpectedHeap = [ nodeSet $ [ (cTwo, [live, dead]) ] ]
        heapSimpleExpectedRegisters =
          [ ("n", livenessN)
          , ("p", liveVal)
          , ("x", livenessX)
          , ("a", liveVal)
          , ("b", deadVal)

          , ("z0", liveVal)
          , ("z1", deadVal)
          , ("_1", nodeSet' [ (cTwo, [dead, dead, dead]) ])
          ]
        livenessN = nodeSet $ [ (cTwo, [live, dead]) ]
        livenessX = livenessN
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapSimpleExpected

  it "heap_update_complex" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- pure (CBool z0)
            n1 <- pure (CWord z0)
            p0 <- case z0 of
              0@alt0 -> store n0
              1@alt1 -> store n1
            n2 <- pure (CNode z0)
            _1 <- update p0 n2
            n3 <- fetch p0
            case n3 of
              (CBool c0) @ alt2 -> pure c0
              (CWord c1) @ alt3 -> pure z0
              (CNode c2) @ alt4 -> pure c2
              (CNope c3) @ alt5 -> pure c3
        |]
    let heapUpdateComplexExpected = emptyLVAResult
          { _memory     = heapUpdateComplexExpectedHeap
          , _registerLv = heapUpdateComplexExpectedRegisters
          , _functionLv = mkFunctionLivenessMap []
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

          , ("z0", liveVal)
          , ("_1", deadVal)
          , ("alt0", deadVal)
          , ("alt1", deadVal)
          , ("alt2", deadNodeSet [ (cBool, 1) ])
          , ("alt3", deadNodeSet [ (cWord, 1) ])
          , ("alt4", deadNodeSet [ (cNode, 1) ])
          , ("alt5", deadVal)
          ]
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cWord, [dead]) ]
        livenessN2 = nodeSet [ (cNode, [live]) ]
        livenessN3 = nodeSet [ (cBool, [live])
                             , (cWord, [dead])
                             , (cNode, [live])
                             ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapUpdateComplexExpected

  it "heap_update_fun_call" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- pure (CBool z0)
            n1 <- pure (CWord z0)
            p0 <- f z0 n0 n1
            n2 <- pure (CNode z0)
            q0 <- g p0 n2
            n3 <- fetch p0
            case n3 of
              (CBool c0) @ alt0 -> pure c0
              (CWord c1) @ alt1 -> pure z0
              (CNode c2) @ alt2 -> pure c2
              (CNope c3) @ alt3 -> pure c3

          f x y z =
            z1 <- pure 0
            case z1 of
              0@alt4 -> store y
              1@alt5 -> store z

          g u v =
            update u v
        |]
    let heapUpdateFunCallExpected = emptyLVAResult
          { _memory     = heapUpdateFunCallExpectedHeap
          , _registerLv = heapUpdateFunCallExpectedRegisters
          , _functionLv = heapUpdateFunCallExpectedFunctions
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

          , ("z0", liveVal)
          , ("z1", liveVal)
          , ("alt0", deadNodeSet [ (cBool, 1) ])
          , ("alt1", deadNodeSet [ (cWord, 1) ])
          , ("alt2", deadNodeSet [ (cNode, 1) ])
          , ("alt3", deadVal)
          , ("alt4", deadVal)
          , ("alt5", deadVal)
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
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapUpdateFunCallExpected

  it "heap_update_local" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- pure (CBool z0)
            n1 <- pure (CWord z0)
            p0 <- store n0
            _1 <- update p0 n1
            n2 <- fetch p0
            case n2 of
              (CBool c0) @ alt0 -> pure c0
              (CWord c1) @ alt1 -> pure z0
              (CNope c2) @ alt2 -> pure c2
        |]
    let heapUpdateLocalExpected = emptyLVAResult
          { _memory     = heapUpdateLocalExpectedHeap
          , _registerLv = heapUpdateLocalExpectedRegisters
          , _functionLv = mkFunctionLivenessMap []
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

          , ("z0", liveVal)
          , ("_1", deadVal)
          , ("alt0", deadNodeSet [ (cBool, 1) ])
          , ("alt1", deadNodeSet [ (cWord, 1) ])
          , ("alt2", deadVal)
          ]
        livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cWord, [dead]) ]
        livenessN2 = nodeSet [ (cBool, [live]), (cWord, [dead]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` heapUpdateLocalExpected

  it "var_pat" $ do
    let exp = [prog|
          grinMain =
            y <- pure 0
            z <- f y
            pure y

          f x = pure x
        |]
    let varPatExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = [ ("z", deadVal), ("y", liveVal), ("x", deadVal) ]
          , _functionLv = varPatExpectedFunctions
          }
        varPatExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (deadVal, [deadVal])) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` varPatExpected

  it "node_pat" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            y <- pure (CInt z0)
            (CInt n) @ _1 <- f y
            pure y

          f x = pure x
        |]
    let nodePatExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = [ ("n", deadVal)
                          , ("y", livenessY)
                          , ("x", livenessFRet)

                          , ("z0", liveVal)
                          , ("_1", nodeSet' [ (cInt, [dead, dead]) ])
                          ]
          , _functionLv = nodePatExpectedFunctions
          }
        nodePatExpectedFunctions =
          [ ("f",        fun (livenessFRet, [livenessFArg]))
          , ("grinMain", fun (livenessY, []))
          ]

        livenessY    = nodeSet [ (cInt, [live]) ]
        livenessFRet = nodeSet [ (cInt, [dead]) ]
        livenessFArg = livenessFRet

        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` nodePatExpected

  it "main_node_ret" $ do
    let exp = [prog|
          grinMain =
            a <- pure 0
            b <- pure 0
            n <- pure (CTwo a b)
            pure n
        |]
    let mainNodeRetExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = mainNodeRetExpectedRegisters
          , _functionLv = [ ("grinMain", fun (livenessN, [])) ]
          }
        mainNodeRetExpectedRegisters =
          [ ("a", liveVal)
          , ("b", liveVal)
          , ("n", livenessN)
          ]
        livenessN = nodeSet [ (cTwo, [live, live]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` mainNodeRetExpected

  it "nodes_simple" $ do
    let exp = [prog|
          grinMain =
            a0 <- pure 0
            a1 <- pure 1
            a2 <- pure 2
            n0 <- f a0 a1 a2
            case n0 of
              (CInt  c0) @ alt0 -> pure c0
              (CBool c1) @ alt1 -> pure 5

          f x y z =
            case x of
              0@alt2 -> pure (CInt  y)
              1@alt3 -> pure (CBool z)
        |]
    let nodesSimpleExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = nodesSimpleExpectedRegisters
          , _functionLv = nodesSimpleExpectedFunctions
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

          , ("alt0", deadNodeSet [ (cInt, 1) ])
          , ("alt1", deadNodeSet [ (cBool, 1) ])
          , ("alt2", deadVal)
          , ("alt3", deadVal)
          ]
        livenessN = nodeSet [ (cFoo, [live]) ]
        nodesSimpleExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessN0, [liveVal, liveVal, deadVal])) ]
        livenessN0 = nodeSet [ (cInt,  [live]), (cBool, [dead]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` nodesSimpleExpected

  it "nodes_tricky" $ do
    pendingWith "illegal grin code: every node tag must have a single arity for the whole program"
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            n0 <- f z0
            case n0 of
              (COne c0 c1) @ alt0 -> pure z0
              (CTwo c2 c3) @ alt1 -> pure c2

          f x =
            case x of
              0@alt2 -> pure (COne x)
              1@alt3 -> pure (CTwo z0 x)
        |]
    let nodesTrickyExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = nodesTrickyExpectedRegisters
          , _functionLv = nodesTrickyExpectedFunctions
          }
        nodesTrickyExpectedRegisters =
          [ ("n0", livenessN0)
          , ("c0", deadVal)
          , ("c1", deadVal)
          , ("c2", liveVal)
          , ("c3", deadVal)
          , ("x",  liveVal)

          , ("z0", liveVal)
          , ("alt0", deadVal)
          , ("alt1", deadVal)
          , ("alt2", deadVal)
          , ("alt3", deadVal)
          ]
        nodesTrickyExpectedFunctions = mkFunctionLivenessMap
          [ ("f", fun (livenessN0, [liveVal])) ]
        livenessN0 = nodeSet [ (cOne, [dead]), (cTwo, [live, dead]) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` nodesTrickyExpected

  -- TODO: new syntax, no conversion
  it "sum_opt" $ do
    let exp = withPrimPrelude [prog|
      grinMain =
        z0 <- pure 0
        z1 <- pure 1
        z2 <- pure 100000
        x.2 <- pure z2
        x.1 <- pure z1
        x.0 <- pure z0
        n13 <- sum $ x.0 x.1 x.2
        x.3 <- pure n13
        _prim_int_print $ x.3

      sum n29 n30 n31 =
        x.5 <- pure n31
        x.4 <- pure n30
        b2 <- _prim_int_gt $ x.4 x.5
        case b2 of
          #True @ alt.0 ->
            pure n29
          #False @ alt.1 ->
            z3 <- pure 1
            x.7 <- pure z3
            x.6 <- pure n30
            n18 <- _prim_int_add $ x.6 x.7
            x.9 <- pure n30
            x.8 <- pure n29
            n28 <- _prim_int_add $ x.8 x.9
            x.12 <- pure n31
            x.11 <- pure n18
            x.10 <- pure n28
            sum $ x.10 x.11 x.12
        |]
    let sumOptExpected = emptyLVAResult
          { _memory     = []
          , _registerLv = sumOptExpectedRegisters
          , _functionLv = sumOptExpectedFunctions
          , _registerEff = sumOptExpectedRegisterEffects
          , _functionEff = sumOptExpectedFunctionEffects
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
          , ("x.0", liveVal)
          , ("x.1", liveVal)
          , ("x.2", liveVal)
          , ("x.3", liveVal)
          , ("x.4", liveVal)
          , ("x.5", liveVal)
          , ("x.6", liveVal)
          , ("x.7", liveVal)
          , ("x.8", liveVal)
          , ("x.9", liveVal)
          , ("x.10", liveVal)
          , ("x.11", liveVal)
          , ("x.12", liveVal)

          , ("z0", liveVal)
          , ("z1", liveVal)
          , ("z2", liveVal)
          , ("z3", liveVal)
          , ("alt.0", deadVal)
          , ("alt.1", deadVal)
          ]
        sumOptExpectedFunctions = mkFunctionLivenessMap
          [ ("sum", fun (liveVal, [liveVal, liveVal, liveVal]))
          ]

        sumOptExpectedRegisterEffects =
          [ ("n13", noEffect)
          , ("n29", noEffect)
          , ("n29", noEffect)
          , ("n30", noEffect)
          , ("n31", noEffect)
          , ("b2",  noEffect)
          , ("n18", noEffect)
          , ("n28", noEffect)
          , ("x.0", noEffect)
          , ("x.1", noEffect)
          , ("x.2", noEffect)
          , ("x.3", noEffect)
          , ("x.4", noEffect)
          , ("x.5", noEffect)
          , ("x.6", noEffect)
          , ("x.7", noEffect)
          , ("x.8", noEffect)
          , ("x.9", noEffect)
          , ("x.10", noEffect)
          , ("x.11", noEffect)
          , ("x.12", noEffect)

          , ("z0", noEffect)
          , ("z1", noEffect)
          , ("z2", noEffect)
          , ("z3", noEffect)
          , ("alt.0", noEffect)
          , ("alt.1", noEffect)
          ]

        sumOptExpectedFunctionEffects =
          [ ("sum",      noEffect)
          , ("grinMain", hasEffect)
          ]

        calculated = calcLiveness exp
    calculated `sameAs` sumOptExpected

  it "undefined" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure 0
            z1 <- pure (CNil)

            p0 <- store z1
            z2 <- pure (CCons z0 p0)
            p1 <- store z2

            x0 <- pure (#undefined :: T_Int64)
            n0 <- pure (#undefined :: {CCons[T_Int64, #ptr]})

            z3 <- pure (#undefined :: T_Int64)
            n1 <- pure (CCons z3 p0)

            z4 <- pure (#undefined :: #ptr)
            n2 <- pure (CCons z4 p0)
            (CCons c0 c1) @ _1 <- pure n2
            x1 <- fetch c0
            pure x1
        |]
    let undefinedExpected = emptyLVAResult
          { _memory     = undefinedExpectedHeap
          , _registerLv = undefinedExpectedRegisters
          , _functionLv = mkFunctionLivenessMap []
          }
        undefinedExpectedHeap =
          [ cNilDead
          , cConsDead
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

          , ("z0", deadVal)
          , ("z1", cNilDead)
          , ("z2", cConsDead)
          , ("z3", deadVal)
          , ("z4", liveVal)
          , ("_1", cConsDead)
          ]
        livenessN0 = cConsDead
        livenessN1 = cConsDead
        livenessN2 = nodeSet [ (cCons, [live, dead]) ]
        cNilDead   = deadNodeSet [ (cNil,  0) ]
        cConsDead  = deadNodeSet [ (cCons, 2) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` undefinedExpected

  it "undefined_with_loc_info" $ do
    let exp = [prog|
          grinMain =
            z0 <- pure (CNil)
            p0 <- store z0
            z1 <- pure (#undefined :: {0})
            n0 <- pure (CCons z1 p0)
            (CCons c0 c1) @ _1 <- pure n0
            x0 <- fetch c0
            pure x0
        |]
    let undefinedWithLocInfoExpected = emptyLVAResult
          { _memory     = [ cNilLiveness ]
          , _registerLv = undefinedWithLocInfoExpectedRegisters
          , _functionLv = undefinedWithLocInfoExpectedFunctions
          }
        undefinedWithLocInfoExpectedRegisters =
          [ ("p0", deadVal)
          , ("x0", cNilLiveness)
          , ("n0", livenessN0)
          , ("c0", liveVal)
          , ("c1", deadVal)

          , ("z0", cNilLiveness)
          , ("z1", liveLoc)
          , ("_1", deadNodeSet [ (cCons, 2) ])
          ]
        undefinedWithLocInfoExpectedFunctions =
          [ ("grinMain", fun (cNilLiveness, [])) ]
        livenessN0 = nodeSet [ (cCons, [live, dead]) ]
        cNilLiveness = nodeSet [ (cNil, []) ]
        calculated = (calcLiveness exp) { _registerEff = mempty, _functionEff = mempty }
    calculated `sameAs` undefinedWithLocInfoExpected

  it "case_lit_pat_side_effect" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            n <- pure 0
            y <- case n of
              0 @ alt0 ->
                z0 <- pure 0
                _prim_int_print z0
              1 @ alt1 ->
                z1 <- pure #"asd"
                _prim_string_print z1
              2 @ alt2 ->
                pure ()
            pure ()
        |]
    let expected = emptyLVAResult
          { _memory      = []
          , _registerLv  = expectedRegisterLiveness
          , _functionLv  = expectedFunctionLiveness
          , _registerEff = expectedRegisterEffects
          , _functionEff = expectedFunctionEffects
          }
        expectedRegisterLiveness =
          [ ("n",  liveVal)
          , ("y",  deadVal)

          , ("z0", liveVal)
          , ("z1", liveVal)
          , ("alt0", deadVal)
          , ("alt1", deadVal)
          , ("alt2", deadVal)
          ]
        expectedFunctionLiveness = mkFunctionLivenessMap []

        expectedRegisterEffects =
          [ ("n",  noEffect)
          , ("y",  hasEffect)

          , ("z0", noEffect)
          , ("z1", noEffect)
          , ("alt0", noEffect)
          , ("alt1", noEffect)
          , ("alt2", noEffect)
          ]

        expectedFunctionEffects =
          [ ("grinMain", hasEffect)
          ]

        calculated = calcLiveness exp
    calculated `sameAs` expected

  it "case_node_pat_side_effect" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            z0 <- pure 0
            n <- pure (COne z0)
            y <- case n of
              (COne c1) @ alt0 ->
                zero <- pure 0
                _prim_int_print zero
              (CTwo c2) @ alt1 ->
                asd <- pure #"asd"
                _prim_string_print asd
              (CFoo c3) @ alt2 ->
                pure ()
            pure ()
        |]
    let expected = emptyLVAResult
          { _memory      = []
          , _registerLv  = expectedRegisterLiveness
          , _functionLv  = expectedFunctionLiveness
          , _registerEff = expectedRegisterEffects
          , _functionEff = expectedFunctionEffects
          }
        expectedRegisterLiveness =
          [ ("n",  nodeSet [ (cOne, [dead]) ])
          , ("y",  deadVal)
          , ("c1", deadVal)
          , ("c2", deadVal)
          , ("c3", deadVal)
          , ("zero", liveVal)
          , ("asd",  deadVal) -- because CTwo does not get analyzed

          , ("z0", deadVal)
          , ("alt0", deadNodeSet [ (cOne, 1) ])
          , ("alt1", deadVal)
          , ("alt2", deadVal)
          ]
        expectedFunctionLiveness = mkFunctionLivenessMap []

        expectedRegisterEffects =
          [ ("n",  noEffect)
          , ("y",  hasEffect)
          , ("c1", noEffect)
          , ("c2", noEffect)
          , ("c3", noEffect)
          , ("zero", noEffect)
          , ("asd",  noEffect)

          , ("z0", noEffect)
          , ("alt0", noEffect)
          , ("alt1", noEffect)
          , ("alt2", noEffect)
          ]

        expectedFunctionEffects =
          [ ("grinMain", hasEffect)
          ]

        calculated = calcLiveness exp
    calculated `sameAs` expected

  it "case_default_alt_side_effect" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            z0 <- pure 0
            n <- pure (COne z0)
            y <- case n of
              (CFoo c2) @ alt0 ->
                pure ()
              #default@alt1 ->
                z1 <- pure 0
                _prim_int_print z1
            pure ()
        |]
    let expected = emptyLVAResult
          { _memory      = []
          , _registerLv  = expectedRegisterLiveness
          , _functionLv  = expectedFunctionLiveness
          , _registerEff = expectedRegisterEffects
          , _functionEff = expectedFunctionEffects
          }
        expectedRegisterLiveness =
          [ ("n",  nodeSet' [ (cOne, [live, dead]) ])
          , ("y",  deadVal)
          , ("c2", deadVal)

          , ("z0", deadVal)
          , ("z1", liveVal)
          , ("alt0", deadVal)
          , ("alt1", deadNodeSet [ (cOne, 1) ])
          ]
        expectedFunctionLiveness = mkFunctionLivenessMap []

        expectedRegisterEffects =
          [ ("n",  noEffect)
          , ("y",  hasEffect)
          , ("c2", noEffect)

          , ("z0", noEffect)
          , ("z1", noEffect)
          , ("alt0", noEffect)
          , ("alt1", noEffect)
          ]

        expectedFunctionEffects =
          [ ("grinMain", hasEffect)
          ]

        calculated = calcLiveness exp
    calculated `sameAs` expected

  it "case_dead_tags_side_effect" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            z0 <- pure 0
            z1 <- pure 1
            n1 <- pure (COne z0)
            p  <- store n1
            n2 <- pure (CTwo z1)
            _1 <- update p n2
            n3 <- fetch p
            y <- case n3 of
              (CTwo c) @ alt0 ->
                z2 <- pure #"asd"
                _prim_string_print z2
            pure ()
        |]
    let expected = emptyLVAResult
          { _memory      = expectedHeapLiveness
          , _registerLv  = expectedRegisterLiveness
          , _functionLv  = expectedFunctionLiveness
          , _registerEff = expectedRegisterEffects
          , _functionEff = expectedFunctionEffects
          }

        livenessN1 = nodeSet' [ (cOne, [dead, dead]) ]
        livenessN2 = nodeSet' [ (cTwo, [live, dead]) ]
        livenessN3 = nodeSet' [ (cOne, [dead, dead]), (cTwo, [live, dead]) ]

        expectedHeapLiveness =
          [ livenessN3
          ]

        expectedRegisterLiveness =
          [ ("n1", livenessN1)
          , ("n2", livenessN2)
          , ("n3", livenessN3)
          , ("p",  liveLoc)
          , ("y",  deadVal)
          , ("c",  deadVal)

          , ("z0", deadVal)
          , ("z1", deadVal)
          , ("z2", liveVal)
          , ("_1", deadVal)
          , ("alt0", deadNodeSet [ (cTwo, 1) ])
          ]

        expectedFunctionLiveness = mkFunctionLivenessMap []

        expectedRegisterEffects =
          [ ("n1", noEffect)
          , ("n2", noEffect)
          , ("n3", noEffect)
          , ("p",  noEffect)
          , ("y",  hasEffect)
          , ("c",  noEffect)

          , ("z0", noEffect)
          , ("z1", noEffect)
          , ("z2", noEffect)
          , ("_1", noEffect)
          , ("alt0", noEffect)
          ]

        expectedFunctionEffects =
          [ ("grinMain", hasEffect)
          ]

        calculated = calcLiveness exp
    calculated `sameAs` expected

  it "case_fun_side_effect" $ do
    let exp = withPrimPrelude [prog|
          grinMain =
            z0 <- pure 0
            n <- pure (COne z0)
            y <- case n of
              (COne c1) @ alt0 ->
                f z0
              (CTwo c2) @ alt1 ->
                asd <- pure #"asd"
                g asd
              (CFoo c3) @ alt2 ->
                h
            pure ()

          f x1 = _prim_int_print x1
          g x2 = _prim_string_print x2
          h    = pure ()
        |]
    let expected = emptyLVAResult
          { _memory      = []
          , _registerLv  = expectedRegisterLiveness
          , _functionLv  = expectedFunctionLiveness
          , _registerEff = expectedRegisterEffects
          , _functionEff = expectedFunctionEffects
          }
        expectedRegisterLiveness =
          [ ("n",   nodeSet [ (cOne, [dead]) ])
          , ("y",   deadVal)
          , ("c1",  deadVal)
          , ("c2",  deadVal)
          , ("c3",  deadVal)
          , ("x1",  liveVal)
          , ("x2",  liveVal)
          , ("asd", deadVal) -- because the CTwo alternative is impossible

          , ("z0", liveVal)
          , ("alt0", deadNodeSet [ (cOne, 1) ])
          , ("alt1", deadVal)
          , ("alt2", deadVal)
          ]
        expectedFunctionLiveness = mkFunctionLivenessMap
          [ ("f", fun (deadVal, [liveVal]))
          , ("g", fun (deadVal, [liveVal]))
          , ("h", fun (deadVal, []))
          ]

        expectedRegisterEffects =
          [ ("n",   noEffect)
          , ("y",   hasEffect)
          , ("c1",  noEffect)
          , ("c2",  noEffect)
          , ("c3",  noEffect)
          , ("x1",  noEffect)
          , ("x2",  noEffect)
          , ("asd", noEffect)

          , ("z0", noEffect)
          , ("alt0", noEffect)
          , ("alt1", noEffect)
          , ("alt2", noEffect)
          ]

        expectedFunctionEffects =
          [ ("f", hasEffect)
          , ("g", hasEffect)
          , ("h", noEffect)
          , ("grinMain", hasEffect)
          ]

        calculated = calcLiveness exp
    calculated `sameAs` expected

  it "case_nested_side_effect" $ do
    let exp = withPrimPrelude [prog|
                grinMain =
                  z0 <- pure 0
                  x <- pure (CInt z0)
                  y <- case x of
                    (CInt n) @ alt0 ->
                      z <- case n of
                        #default @ alt1 ->
                          _v <- _prim_int_print z0
                          pure ()
                      pure z
                  pure 0
              |]
    let expected = emptyLVAResult
          { _memory      = []
          , _registerLv  = expectedRegisterLiveness
          , _functionLv  = expectedFunctionLiveness
          , _registerEff = expectedRegisterEffects
          , _functionEff = expectedFunctionEffects
          }
        expectedRegisterLiveness =
          [ ("x", nodeSet [ (cInt, [live]) ])
          , ("y", deadVal)
          , ("z", deadVal)
          , ("n", liveVal)
          , ("_v", deadVal)

          , ("z0", liveVal)
          , ("alt0", deadNodeSet [ (cInt, 1) ])
          , ("alt1", deadVal)
          ]
        expectedFunctionLiveness = mkFunctionLivenessMap []

        expectedRegisterEffects =
          [ ("x", noEffect)
          , ("y", hasEffect)
          , ("z", hasEffect)
          , ("n", noEffect)
          , ("_v", hasEffect)

          , ("z0", noEffect)
          , ("alt0", noEffect)
          , ("alt1", noEffect)
          ]

        expectedFunctionEffects =
          [ ("grinMain", hasEffect)
          ]

        calculated = calcLiveness exp
    calculated `sameAs` expected

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

hasEffect :: Effect
hasEffect = Effect True

noEffect :: Effect
noEffect = Effect False
