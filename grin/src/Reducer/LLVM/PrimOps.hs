module Reducer.LLVM.PrimOps where

import LLVM.AST
import qualified LLVM.AST.IntegerPredicate as I
import qualified LLVM.AST.FloatingPointPredicate as F
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Type
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C

import Reducer.LLVM.Base

codeGenPrimOp name _ [opA, opB] = case name of
  -- Int
  "_prim_int_add"   -> pure . I $ Add  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_sub"   -> pure . I $ Sub  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_mul"   -> pure . I $ Mul  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_div"   -> pure . I $ SDiv {exact=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_eq"    -> pure . I $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_ne"    -> pure . I $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_gt"    -> pure . I $ ICmp {iPredicate=I.SGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_ge"    -> pure . I $ ICmp {iPredicate=I.SGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_lt"    -> pure . I $ ICmp {iPredicate=I.SLT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_le"    -> pure . I $ ICmp {iPredicate=I.SLE, operand0=opA, operand1=opB, metadata=[]}

  -- Word
  "_prim_word_add"  -> pure . I $ Add  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_sub"  -> pure . I $ Sub  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_mul"  -> pure . I $ Mul  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_div"  -> pure . I $ UDiv {exact=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_eq"   -> pure . I $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_ne"   -> pure . I $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_gt"   -> pure . I $ ICmp {iPredicate=I.UGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_ge"   -> pure . I $ ICmp {iPredicate=I.UGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_lt"   -> pure . I $ ICmp {iPredicate=I.ULT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_le"   -> pure . I $ ICmp {iPredicate=I.ULE, operand0=opA, operand1=opB, metadata=[]}

  -- Float
  "_prim_float_add" -> pure . I $ FAdd {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_sub" -> pure . I $ FSub {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_mul" -> pure . I $ FMul {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_div" -> pure . I $ FDiv {fastMathFlags=NoFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_eq"  -> pure . I $ FCmp {fpPredicate=F.OEQ, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_ne"  -> pure . I $ FCmp {fpPredicate=F.ONE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_gt"  -> pure . I $ FCmp {fpPredicate=F.OGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_ge"  -> pure . I $ FCmp {fpPredicate=F.OGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_lt"  -> pure . I $ FCmp {fpPredicate=F.OLT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_le"  -> pure . I $ FCmp {fpPredicate=F.OLE, operand0=opA, operand1=opB, metadata=[]}

codeGenPrimOp "_prim_int_print" _ [opA] = pure . I $ Call
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
