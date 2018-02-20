module Reducer.LLVM.PrimOps where

import LLVM.AST
import qualified LLVM.AST.IntegerPredicate as I
import qualified LLVM.AST.FloatingPointPredicate as F
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Type as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C

import Reducer.LLVM.Base

codeGenPrimOp name _ [opA, opB] = case name of
  -- Int
  "_prim_int_add"   -> pure . I i64 $ Add  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_sub"   -> pure . I i64 $ Sub  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_mul"   -> pure . I i64 $ Mul  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_div"   -> pure . I i64 $ SDiv {exact=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_eq"    -> pure . I i64 $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_ne"    -> pure . I i64 $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_gt"    -> pure . I i64 $ ICmp {iPredicate=I.SGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_ge"    -> pure . I i64 $ ICmp {iPredicate=I.SGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_lt"    -> pure . I i64 $ ICmp {iPredicate=I.SLT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_le"    -> pure . I i64 $ ICmp {iPredicate=I.SLE, operand0=opA, operand1=opB, metadata=[]}

  -- Word
  "_prim_word_add"  -> pure . I i64 $ Add  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_sub"  -> pure . I i64 $ Sub  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_mul"  -> pure . I i64 $ Mul  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_div"  -> pure . I i64 $ UDiv {exact=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_eq"   -> pure . I i64 $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_ne"   -> pure . I i64 $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_gt"   -> pure . I i64 $ ICmp {iPredicate=I.UGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_ge"   -> pure . I i64 $ ICmp {iPredicate=I.UGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_lt"   -> pure . I i64 $ ICmp {iPredicate=I.ULT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_le"   -> pure . I i64 $ ICmp {iPredicate=I.ULE, operand0=opA, operand1=opB, metadata=[]}

  -- Float
  "_prim_float_add" -> pure . I float $ FAdd {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_sub" -> pure . I float $ FSub {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_mul" -> pure . I float $ FMul {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_div" -> pure . I float $ FDiv {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_eq"  -> pure . I float $ FCmp {fpPredicate=F.OEQ, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_ne"  -> pure . I float $ FCmp {fpPredicate=F.ONE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_gt"  -> pure . I float $ FCmp {fpPredicate=F.OGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_ge"  -> pure . I float $ FCmp {fpPredicate=F.OGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_lt"  -> pure . I float $ FCmp {fpPredicate=F.OLT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_le"  -> pure . I float $ FCmp {fpPredicate=F.OLE, operand0=opA, operand1=opB, metadata=[]}

  -- Bool
  "_prim_bool_eq"   -> pure . I i1 $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_bool_ne"   -> pure . I i1 $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}

codeGenPrimOp "_prim_int_print" _ [opA] = pure . I i64 $ Call
    { tailCallKind        = Nothing
    , callingConvention   = CC.C
    , returnAttributes    = []
    , function            = Right $ ConstantOperand $ C.GlobalReference (fun i64 [i64]) (mkName "_prim_int_print")
    , arguments           = [(opA, [])]
    , functionAttributes  = []
    , metadata            = []
    }
  where
    ptr ty = PointerType { pointerReferent = ty, pointerAddrSpace = AddrSpace 0}
    fun ret args = ptr FunctionType {resultType = ret, argumentTypes = args, isVarArg = False}

codeGenPrimOp name args _ = error $ "unknown primitive operation: " ++ name ++ " arguments: " ++ show args
