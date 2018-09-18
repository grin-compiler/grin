{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Frontend.Lambda.PrimOps where

import Grin.TH

lambdaPrimOps = [prog|
  {-
  ap ap_f ap_x =
    ap_v <- eval ap_f
    apply ap_v ap_x
  -}
  {-
  _rts_int_gt p0$ p1$ =
    (CInt int0$) <- eval p0$
    (CInt int1$) <- eval p1$
    bool0$ <- _prim_int_gt int0$ int1$
    case bool0$ of
      #False  -> pure (CFalse)
      #True   -> pure (CTrue)

  _rts_int_print p2$ =
    (CInt int2$) <- eval p2$
    _prim_int_print int2$
    pure (CUnit)

  _rts_int_add p3$ p4$ =
    (CInt int3$) <- eval p3$
    (CInt int4$) <- eval p4$
    int5$ <- _prim_int_add int3$ int4$
    pure (CInt int5$)
  -}
|]
