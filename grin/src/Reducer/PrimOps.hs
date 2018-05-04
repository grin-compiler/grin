module Reducer.PrimOps (evalPrimOp) where

import Grin
import Data.Map.Strict as Map
import Control.Monad.IO.Class

-- primitive functions
primIntPrint [Lit (LInt64 a)] = liftIO (print a) >> pure (Lit $ LInt64 a)
primIntPrint x = error $ "primIntPrint - invalid arguments: " ++ show x

evalPrimOp name args = case name of
  "_prim_int_print" -> primIntPrint args
  -- Int
  "_prim_int_add"   -> int_bin_op int (+)
  "_prim_int_sub"   -> int_bin_op int (-)
  "_prim_int_mul"   -> int_bin_op int (*)
  "_prim_int_div"   -> int_bin_op int div
  "_prim_int_eq"    -> int_bin_op bool (==)
  "_prim_int_ne"    -> int_bin_op bool (/=)
  "_prim_int_gt"    -> int_bin_op bool (>)
  "_prim_int_ge"    -> int_bin_op bool (>=)
  "_prim_int_lt"    -> int_bin_op bool (<)
  "_prim_int_le"    -> int_bin_op bool (<=)
  -- Word
  "_prim_word_add"  -> word_bin_op word (+)
  "_prim_word_sub"  -> word_bin_op word (-)
  "_prim_word_mul"  -> word_bin_op word (*)
  "_prim_word_div"  -> word_bin_op word div
  "_prim_word_eq"   -> word_bin_op bool (==)
  "_prim_word_ne"   -> word_bin_op bool (/=)
  "_prim_word_gt"   -> word_bin_op bool (>)
  "_prim_word_ge"   -> word_bin_op bool (>=)
  "_prim_word_lt"   -> word_bin_op bool (<)
  "_prim_word_le"   -> word_bin_op bool (<=)
  -- Float
  "_prim_float_add" -> float_bin_op float (+)
  "_prim_float_sub" -> float_bin_op float (-)
  "_prim_float_mul" -> float_bin_op float (*)
  "_prim_float_div" -> float_bin_op float (/)
  "_prim_float_eq"  -> float_bin_op bool (==)
  "_prim_float_ne"  -> float_bin_op bool (/=)
  "_prim_float_gt"  -> float_bin_op bool (>)
  "_prim_float_ge"  -> float_bin_op bool (>=)
  "_prim_float_lt"  -> float_bin_op bool (<)
  "_prim_float_le"  -> float_bin_op bool (<=)
  -- Bool
  "_prim_bool_eq"   -> bool_bin_op bool (==)
  "_prim_bool_ne"   -> bool_bin_op bool (/=)

  _ -> error $ "unknown primitive operation: " ++ name
 where
  int   x = pure . Lit . LInt64 $ x
  word  x = pure . Lit . LWord64 $ x
  float x = pure . Lit . LFloat $ x
  bool  x = pure . Lit . LBool $ x

  int_bin_op retTy fn = case args of
    [Lit (LInt64 a), Lit (LInt64 b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show args ++ " for " ++ name

  word_bin_op retTy fn = case args of
    [Lit (LWord64 a), Lit (LWord64 b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show args ++ " for " ++ name

  float_bin_op retTy fn = case args of
    [Lit (LFloat a), Lit (LFloat b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show args ++ " for " ++ name

  bool_bin_op retTy fn = case args of
    [Lit (LBool a), Lit (LBool b)] -> retTy $ fn a b
    _ -> error $ "invalid arguments: " ++ show args ++ " for " ++ name
