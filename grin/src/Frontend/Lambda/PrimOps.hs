{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Frontend.Lambda.PrimOps where

import GrinTH

lambdaPrimOps = [prog|
  ap ap_f ap_x =
    ap_v <- eval ap_f
    apply ap_v ap_x

  int_gt int_gt0 int_gt1 =
    (CInt int_gt0_1) <- eval int_gt0
    (CInt int_gt1_1) <- eval int_gt1
    int_gt2 <- _prim_int_gt int_gt0_1 int_gt1_1
    case int_gt2 of
      #False  -> pure (CFalse)
      #True   -> pure (CTrue)

  int_print int_print0 =
    (CInt int_print0_1) <- eval int_print0
    _prim_int_print int_print0_1
    pure (CUnit)

  int_add int_add0 int_add1 =
    (CInt int_add0_1) <- eval int_add0
    (CInt int_add1_1) <- eval int_add1
    int_add2 <- _prim_int_add int_add0_1 int_add1_1
    pure (CInt int_add2)
|]
