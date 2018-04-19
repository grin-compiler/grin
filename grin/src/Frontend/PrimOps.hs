{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Frontend.PrimOps where

import GrinTH

lambdaPrimOps = [prog|
  int_eq int_eq0 int_eq1 =
    (CInt int_eq0_1) <- eval int_eq0
    (CInt int_eq1_1) <- eval int_eq1
    int_eq2 <- _prim_int_eq int_eq0_1 int_eq1_1
    case int_eq2 of
      #False  -> pure (CFalse)
      #True   -> pure (CTrue)

  int_lt int_lt0 int_lt1 =
    (CInt int_lt0_1) <- eval int_lt0
    (CInt int_lt1_1) <- eval int_lt1
    int_lt2 <- _prim_int_lt int_lt0_1 int_lt1_1
    case int_lt2 of
      #False  -> pure (CFalse)
      #True   -> pure (CTrue)

  int_le int_le0 int_le1 =
    (CInt int_le0_1) <- eval int_le0
    (CInt int_le1_1) <- eval int_le1
    int_le2 <- _prim_int_le int_le0_1 int_le1_1
    case int_le2 of
      #False -> pure (CFalse)
      #True  -> pure (CTrue)

  int_gt int_gt0 int_gt1 =
    (CInt int_gt0_1) <- eval int_gt0
    (CInt int_gt1_1) <- eval int_gt1
    int_gt2 <- _prim_int_gt int_gt0_1 int_gt1_1
    case int_gt2 of
      #False  -> pure (CFalse)
      #True   -> pure (CTrue)

  int_ge int_ge0 int_ge1 =
    (CInt int_ge0_1) <- eval int_ge0
    (CInt int_ge1_1) <- eval int_ge1
    int_ge2 <- _prim_int_ge int_ge0_1 int_ge1_1
    case int_ge2 of
      #False -> pure (CFalse)
      #True  -> pure (CTrue)

  int_print int_print0 =
    (CInt int_print0_1) <- eval int_print0
    _prim_int_print int_print0_1
    pure (CUnit)

  int_add int_add0 int_add1 =
    (CInt int_add0_1) <- eval int_add0
    (CInt int_add1_1) <- eval int_add1
    int_add2 <- _prim_int_add int_add0_1 int_add1_1
    pure (CInt int_add2)

  int_sub int_sub0 int_sub1 =
    (CInt int_sub0_1) <- eval int_sub0
    (CInt int_sub1_1) <- eval int_sub1
    int_sub2 <- _prim_int_sub int_sub0_1 int_sub1_1
    pure (CInt int_sub2)

  int_mul int_mul0 int_mul1 =
    (CInt int_mul0_1) <- eval int_mul0
    (CInt int_mul1_1) <- eval int_mul1
    int_mul2 <- _prim_int_mul int_mul0_1 int_mul1_1
    pure (CInt int_mul2)

  int_div int_div0 int_div1 =
    (CInt int_div0_1) <- eval int_div0
    (CInt int_div1_1) <- eval int_div1
    int_div2 <- _prim_int_div int_div0_1 int_div1_1
    pure (CInt int_div2)
|]
