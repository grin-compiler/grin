{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Samples.ArityFullRemoveSpec where

import Pipeline

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "multi indirection - full remove" $ do
    let before = [prog|
        grinMain =
          p2 <- store (CInt 1)
          p3 <- store (CInt 1000)
          p4 <- store (Fupto p2 p3)
          n13' <- sum 0 p4
          _prim_int_print n13'

        sum p101 p11 =
          (Fupto p17 p18) <- fetch p11
          (CInt n2') <- fetch p17
          (CInt n3') <- fetch p18
          b1' <- _prim_int_gt n2' n3'
          case b1' of
            #True ->
              pure p101
            #False ->
              n4' <- _prim_int_add n2' 1
              p8 <- store (CInt n4')
              p9 <- store (Fupto p8 p18)
              n7'_2 <- _prim_int_add p101 n2'
              sum n7'_2 p9
      |]
    let after = [prog|
        grinMain =
          n13' <- sum 0 1 1000
          _prim_int_print n13'

        sum p101 p111 p112 =
          b1' <- _prim_int_gt p111 p112
          case b1' of
            #True ->
              pure p101
            #False ->
              n4' <- _prim_int_add p111 1
              n7'_2 <- _prim_int_add p101 p111
              sum n7'_2 n4' p112
      |]
    let ppln =
          [ Pass [HPT CompileHPT, HPT RunHPTPure]
          , T ArityRaising
          , T CopyPropagation
          , T DeadVariableElimination
          , T ArityRaising
          , T CopyPropagation
          , T DeadVariableElimination
          ]
    (pipelineInfo, transformed) <- pipeline defaultOpts before ppln
    transformed `sameAs` after
