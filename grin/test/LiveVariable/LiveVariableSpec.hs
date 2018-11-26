module LiveVariable.LiveVariableSpec where

import System.FilePath

import Grin.Grin

import Test.IO
import Test.Hspec

import AbstractInterpretation.Reduce (AbstractInterpretationResult(..),evalDataFlowInfo)
import AbstractInterpretation.LiveVariable
import AbstractInterpretation.LVAResult

import LiveVariable.Tests.Util
import LiveVariable.Tests.CaseAnonymous
import LiveVariable.Tests.CaseMinLit
import LiveVariable.Tests.CaseMinNodes
import LiveVariable.Tests.CaseNested
import LiveVariable.Tests.CaseRestricted
import LiveVariable.Tests.CaseRestrictedNodes
import LiveVariable.Tests.DeadTags
import LiveVariable.Tests.Fields
import LiveVariable.Tests.FunctionCall1
import LiveVariable.Tests.FunctionCall2
import LiveVariable.Tests.HeapCaseMin
import LiveVariable.Tests.HeapCase
import LiveVariable.Tests.HeapIndirectSimple
import LiveVariable.Tests.HeapSimple
import LiveVariable.Tests.HeapUpdateComplex
import LiveVariable.Tests.HeapUpdateFunCall
import LiveVariable.Tests.HeapUpdateLocal
import LiveVariable.Tests.LitPat
import LiveVariable.Tests.MainNodeRet
import LiveVariable.Tests.NodesSimple
import LiveVariable.Tests.NodesTricky
import LiveVariable.Tests.SumOpt
import LiveVariable.Tests.Undefined
import LiveVariable.Tests.UndefinedWithLocInfo

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

lvaTestName :: String 
lvaTestName = "Live Variable Analysis"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = testGroup lvaTestName $
  mkSpecFromWith fromCurDir calcLiveness
    [ caseAnonymousSrc
    , caseMinLitSrc
    , caseMinNodesSrc
    , caseNestedSrc
    , caseRestrictedSrc
    , caseRestrictedNodesSrc
    , deadTagsSrc
    , fieldsSrc
    , functionCall1Src
    , functionCall2Src
    , heapCaseMinSrc
    , heapCaseSrc
    , heapIndirectSimpleSrc
    , heapSimpleSrc
    , heapUpdateComplexSrc
    , heapUpdateFunCallSrc
    , heapUpdateLocalSrc
    , litPatSrc
    , mainNodeRetSrc
    , nodesSimpleSrc
    , nodesTrickySrc
    , sumOptSrc
    , undefinedSrc
    , undefinedWithLocInfoSrc
    ]
    [ caseAnonymousSpec
    , caseMinLitSpec
    , caseMinNodesSpec
    , caseNestedSpec
    , caseRestrictedSpec
    , caseRestrictedNodesSpec
    , deadTagsSpec
    , fieldsSpec
    , functionCall1Spec
    , functionCall2Spec
    , heapCaseMinSpec
    , heapCaseSpec
    , heapIndirectSimpleSpec
    , heapSimpleSpec
    , heapUpdateComplexSpec
    , heapUpdateFunCallSpec
    , heapUpdateLocalSpec
    , litPatSpec
    , mainNodeRetSpec
    , nodesSimpleSpec
    , nodesTrickySpec
    , sumOptSpec
    , undefinedSpec
    , undefinedWithLocInfoSpec
    ]

calcLiveness :: Exp -> LVAResult
calcLiveness prog
  | Right lvaProgram <- codeGen prog
  , computer <- _airComp . evalDataFlowInfo $ lvaProgram
  = toLVAResult lvaProgram computer
