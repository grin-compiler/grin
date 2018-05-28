{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.Optimising.ArityRaisingSpec where

import Transformations.Optimising.ArityRaising

import Test.Hspec
import Grin
import GrinTH
import Test hiding (newVar)
import Assertions
import TypeEnv
import TypeCheck
import Data.Monoid
import Control.Arrow
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "Step 1" $ do
    let teBefore = emptyTypeEnv
          { _function = Map.fromList
              [ ("non", (int64_t, Vector.fromList [int64_t]))
              , ("one", (int64_t, Vector.fromList [location_t [0]]))
              , ("two", (int64_t, Vector.fromList [location_t [0], int64_t, location_t [1]]))
              , ("bad", (int64_t, Vector.fromList [bool_t, location_t [0, 1]]))
              ]
          , _location = Vector.fromList
              [ (Map.fromList
                  [(Tag C "Int", Vector.fromList [T_Int64])
                  ])
              , (Map.fromList
                  [(Tag C "Float", Vector.fromList [T_Float])
                  ])
              ]
          }
    let before = [prog|
        non x0 =
          y0 <- prim_int_add x0 1
          pure y0

        one pi1 =
          (CInt i1) <- fetch pi1
          pure i1

        two pi2 a2 pf2 =
          (CInt i1)   <- fetch pi1
          (CFloat f2) <- fetch pf2
          pure i1

        bad i3 p3 =
          case i3 of
            #True -> (CInt x3) <- fetch p3
                     y3 <- prim_int_add x3 1
                     pure y3
            #False -> (CFloat x4) <- fetch p3
                      y4 <- round x4
                      pure x4
      |]
    examineTheParameters (teBefore, before) `shouldBe`
      (Map.fromList
        [ ("one", [ ("pi1", 1, (Tag C "Int", Vector.fromList [T_Int64]))] )
        , ("two", [ ("pi2", 1, (Tag C "Int", Vector.fromList [T_Int64]))
                  , ("pf2", 3, (Tag C "Float", Vector.fromList [T_Float]))
                  ])
        ])

  it "Step 2" $ do
    let teBefore = emptyTypeEnv
          { _function = Map.fromList
              [ ("foo", (location_t [0], Vector.fromList [location_t [1]])) ]
          , _location = Vector.fromList
              [ (Map.fromList
                  [(Tag C "Cons", Vector.fromList [T_Location [0], T_Location [1]])])
              , (Map.fromList
                  [(Tag C "Nil", Vector.fromList [])])
              ]
          }
    let before = [prog|
        foo p =
          x <- prim_int_add 1 2
          r <- store (CCons p q)
          pure r
      |]
    let paramMap = examineTheParameters (teBefore, before)
    examineCallees paramMap (teBefore, before) `shouldBe` mempty

  it "Full transformation" $ do
    let teBefore = emptyTypeEnv
          { _function =
              fun_t "foo"  [int64_t, location_t [0]] int64_t <>
              fun_t "bar" [int64_t] int64_t <>
              fun_t "foo2" [location_t [0], int64_t, location_t [0]] int64_t <>
              fun_t "bar2" [int64_t] int64_t
          , _location = Vector.fromList
              [ cnode_t "Bar" [T_Int64, T_Int64]
              ]
          }
    let before = [prog|
        foo x1 y1 =
          z1 <- prim_int_add x1 1
          (CBar r1 s1) <- fetch y1
          prim_int_add r1 s1

        bar x2 =
          z2 <- prim_int_add x2 1
          y2 <- store (CBar 1 z2)
          foo 1 y2

        foo2 x3 y3 z3 =
          w3 <- prim_int_add y3 1
          (CBar w1 w2) <- fetch x3
          (CBar w3 w4) <- fetch z3
          w5 <- prim_int_add w1 w2
          w6 <- prim_int_add w3 w4
          prim_int_add w5 w6

        bar2 x4 =
          z4 <- prim_int_add x4 1
          y4 <- store (CBar 1 z4)
          w4 <- store (CBar 2 z4)
          foo2 y4 1 w4
      |]
    let teAfter = teBefore
          { _variable = Map.fromList
              [ ("y11", int64_t)
              , ("y12", int64_t)
              , ("x31", int64_t)
              , ("x32", int64_t)
              , ("z31", int64_t)
              , ("z32", int64_t)
              , ("x1", int64_t)
              , ("y3", int64_t)
              ]
          , _function =
              fun_t "foo" [int64_t, int64_t, int64_t] int64_t <>
              fun_t "bar" [int64_t] int64_t <>
              fun_t "foo2" [int64_t, int64_t, int64_t, int64_t, int64_t] int64_t <>
              fun_t "bar2" [int64_t] int64_t
          }
    let after = [prog|
        foo x1 y11 y12 =
          z1 <- prim_int_add x1 1
          (CBar r1 s1) <- pure (CBar y11 y12)
          prim_int_add r1 s1

        bar x2 =
          z2 <- prim_int_add x2 1
          y2 <- store (CBar 1 z2)
          foo 1 1 z2

        foo2 x31 x32 y3 z31 z32 =
          w3 <- prim_int_add y3 1
          (CBar w1 w2) <- pure (CBar x31 x32)
          (CBar w3 w4) <- pure (CBar z31 z32)
          w5 <- prim_int_add w1 w2
          w6 <- prim_int_add w3 w4
          prim_int_add w5 w6

        bar2 x4 =
          z4 <- prim_int_add x4 1
          y4 <- store (CBar 1 z4)
          w4 <- store (CBar 2 z4)
          foo2 1 z4 1 2 z4
      |]
    (arityRaising (teBefore, before)) `sameAs` (teAfter, after)

  it "Zero arguments" $ do
    let teBefore = emptyTypeEnv
          { _function =
              fun_t "empty_node_fn" [location_t [0]] int64_t <>
              fun_t "use_empty_node" [location_t [0]] unit_t
          , _location = Vector.fromList
              [ cnode_t "Void" []
              ]
          }
    let before = [prog|
        empty_node_fn pv0 =
          (CVoid) <- fetch pv0
          pure 0

        use_empty_node pv1 =
          i1 <- empty_node_fn pv1
          prim_print_int i1
      |]
    let teAfter = teBefore
    let after = [prog|
        empty_node_fn pv0 =
          (CVoid) <- fetch pv0
          pure 0

        use_empty_node pv1 =
          i1 <- empty_node_fn pv1
          prim_print_int i1
      |]
    arityRaising (teBefore, before) `sameAs` (teAfter, after)

  it "bugfix - leaving unknown variable behind" $ do
    let before = [prog|
        grinMain =
          p2 <- store (CInt 1)
          n13' <- sum #False p2
          _prim_int_print n13'

        sum b2' p30 =
          (CInt n17') <- fetch p30
          case b2' of
            #True ->
              pure n17'
            #False ->
              sum #True p30
      |]
    let after = [prog|
        grinMain =
          p2 <- store (CInt 1)
          n13' <- sum #False 1
          _prim_int_print n13'

        sum b2' p30 =
          (CInt n17') <- pure (CInt p30)
          case b2' of
            #True ->
              pure n17'
            #False ->
              sum #True p30
      |]
    snd (arityRaising (inferTypeEnv before, before)) `sameAs` after

  it "bugfix - parameter type error" $ do
    let before = [prog|
        grinMain =
          p1 <- store (CInt 1)
          p4 <- store (Fupto p1)
          n13' <- sum p1 p4
          _prim_int_print n13'

        sum p10 p11 =
          (Fupto p6) <- fetch p11
          (CInt n2') <- fetch p6
          b1' <- _prim_int_gt n2' 0
          case b1' of
            #True ->
              pure 1
            #False ->
              p8 <- store (CInt n2')
              p9 <- store (Fupto p8)
              sum p10 p9
      |]
    let after = [prog|
        grinMain =
          p1 <- store (CInt 1)
          p4 <- store (Fupto p1)
          n13' <- sum 1 p1
          _prim_int_print n13'

        sum p10 p11 =
          (Fupto p6) <- pure (Fupto p11)
          (CInt n2') <- fetch p6
          b1' <- _prim_int_gt n2' 0
          case b1' of
            #True ->
              pure 1
            #False ->
              p8 <- store (CInt n2')
              p9 <- store (Fupto p8)
              sum p10 p8
      |]
    snd (arityRaising (inferTypeEnv before, before)) `sameAs` after

  it "bugfix - multi call" $ do
    let before = [prog|
        sub' p1 p2 =
          (CGrInt n1) <- fetch p1
          (CGrInt n2) <- fetch p2
          _prim_int_sub n1 n2

        test' t1 =
          p3 <- store (CGrInt t1)
          p4 <- store (CGrInt 1)
          v2' <- sub' p3 p4
          p5 <- store (CGrInt v2')
          sub' p3 p5

        grinMain =
          m' <- test' 10
          _prim_int_print m'
      |]
    let after = [prog|
        sub' p1 p2 =
          (CGrInt n1) <- pure (CGrInt p1)
          (CGrInt n2) <- pure (CGrInt p2)
          _prim_int_sub n1 n2

        test' t1 =
          p3 <- store (CGrInt t1)
          p4 <- store (CGrInt 1)
          v2' <- sub' t1 1
          p5 <- store (CGrInt v2')
          sub' t1 v2'

        grinMain =
          m' <- test' 10
          _prim_int_print m'
      |]
    snd (arityRaising (inferTypeEnv before, before)) `sameAs` after

  it "bugfix - multi indirection" $ do
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
          p2 <- store (CInt 1)
          p3 <- store (CInt 1000)
          p4 <- store (Fupto p2 p3)
          n13' <- sum 0 p2 p3
          _prim_int_print n13'

        sum p101 p111 p112 =
          (Fupto p17 p18) <- pure (Fupto p111 p112)
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
              sum n7'_2 p8 p18
      |]
    snd (arityRaising (inferTypeEnv before, before)) `sameAs` after

  it "bugfix - update" $ do
    let before = [prog|
        grinMain =
          p1 <- store (CInt 0)
          p2 <- store (CInt 1)
          p3 <- store (CInt 1000)
          p4 <- store (Fupto p2 p3)
          p5 <- store (Fsum p1 p4)
          e_v3.0' <- sum' p1 p4
          e_v3.0 <- pure (CInt e_v3.0')
          update p5 e_v3.0
          _prim_int_print e_v3.0'
        
        upto p6 p7 =
          e_v1.1 <- fetch p6
          (CInt n2') <- pure e_v1.1
          e_v1.2 <- fetch p7
          (CInt n3') <- pure e_v1.2
          b1' <- _prim_int_gt n2' n3'
          case b1' of
            #True ->
              pure (CNil)
            #False ->
              n4' <- _prim_int_add n2' 1
              p8 <- store (CInt n4')
              p9 <- store (Fupto p8 p7)
              pure (CCons p6 p9)
        
        sum' p10 p11 =
          e_v1.3 <- fetch p11
          v1 <- case e_v1.3 of
            #default ->
              pure e_v1.3
            (Fupto e_p2.3 e_p3.3) ->
              e_v2.3 <- upto e_p2.3 e_p3.3
              update p11 e_v2.3
              pure e_v2.3
          case v1 of
            (CNil) ->
              e_v1.4 <- fetch p10
              (CInt e_v1.4') <- pure e_v1.4
              pure e_v1.4'
            (CCons p12 p13) ->
              e_v1.5 <- fetch p10
              (CInt n5') <- pure e_v1.5
              e_v1.6 <- fetch p12
              (CInt n6') <- pure e_v1.6
              n7' <- _prim_int_add n5' n6'
              p14 <- store (CInt n7')
              sum' p14 p13
      |]
    let after = [prog|
      grinMain =
        p1 <- store (CInt 0)
        p2 <- store (CInt 1)
        p3 <- store (CInt 1000)
        p4 <- store (Fupto p2 p3)
        p5 <- store (Fsum p1 p4)
        e_v3.0' <- sum' 0 p4
        e_v3.0 <- pure (CInt e_v3.0')
        update p5 e_v3.0
        _prim_int_print e_v3.0'
      
      upto p6 p7 =
        e_v1.1 <- fetch p6
        (CInt n2') <- pure e_v1.1
        e_v1.2 <- fetch p7
        (CInt n3') <- pure e_v1.2
        b1' <- _prim_int_gt n2' n3'
        case b1' of
          #True ->
            pure (CNil)
          #False ->
            n4' <- _prim_int_add n2' 1
            p8 <- store (CInt n4')
            p9 <- store (Fupto p8 p7)
            pure (CCons p6 p9)
      
      sum' p10 p11 =
        e_v1.3 <- fetch p11
        v1 <- case e_v1.3 of
          #default ->
            pure e_v1.3
          (Fupto e_p2.3 e_p3.3) ->
            e_v2.3 <- upto e_p2.3 e_p3.3
            update p11 e_v2.3
            pure e_v2.3
        case v1 of
          (CNil) ->
            e_v1.4 <- pure (CInt p10)
            (CInt e_v1.4') <- pure e_v1.4
            pure e_v1.4'
          (CCons p12 p13) ->
            e_v1.5 <- pure (CInt p10)
            (CInt n5') <- pure e_v1.5
            e_v1.6 <- fetch p12
            (CInt n6') <- pure e_v1.6
            n7' <- _prim_int_add n5' n6'
            p14 <- store (CInt n7')
            sum' n7' p13
      |]
    snd (arityRaising (inferTypeEnv before, before)) `sameAs` after
