{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module AbstractInterpretation.ExtendedSyntax.SharingSpec where

import Data.Set (Set)
import qualified Data.Set as Set

import Test.Hspec
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.PrimOpsPrelude

import AbstractInterpretation.ExtendedSyntax.Reduce
import AbstractInterpretation.ExtendedSyntax.Sharing.CodeGen
import AbstractInterpretation.ExtendedSyntax.Sharing.Result


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = describe "Sharing analysis" $ do
  it "has not changed for sum simple." $ do
    let result = calcSharedLocations testProgram
    -- TODO: improve precision, improve altName tracking
    let expected = Set.fromList [0,1,2,4,5]
    result `shouldBe` expected

  it "finds non-transitive shared locations" $ do
    -- A sharing is calculated via for non-linear variables
    let code = [prog|
          main =
            one <- pure (COne)
            l0 <- store one
            l1 <- store one
            _1 <- update l0 one
            _2 <- fun l0
            v <- fetch l0
            pure ()
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [0]
    result `shouldBe` expected

  it "finds location based transitive shared location" $ do
    let code = [prog|
          main =
            one <- pure (COne)
            l0 <- store one
            two <- pure (CTwo l0)
            l1 <- store two
            _1 <- update l0 one
            _2 <- fun l1
            v <- fetch l1
            pure ()
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [0,1]
    result `shouldBe` expected

  it "finds location based on transitive non-linear var" $ do
    let code = [prog|
          main =
            one <- pure (COne)
            l0 <- store one
            l1 <- store one
            _1 <- update l0 one
            l2 <- pure l1
            v <- fetch l2
            _2 <- fun l2
            pure ()
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [1]
    result `shouldBe` expected

  it "finds location fetched twice inside a node with as-pattern" $ do
    let code = [prog|
          main =
            one <- pure (COne)
            l0  <- store one
            two <- pure (CTwo l0)
            l1  <- store two
            (CTwo l2) @ _1 <- fetch l1
            _2 <- fetch l2
            _3 <- fetch l2
            pure ()
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [0]
    result `shouldBe` expected

  it "finds location inside shared node - simple" $ do
    let code = [prog|
          grinMain =
            n1 <- pure (COne)
            n2 <- pure (CTwo)
            p1 <- store n1
            n3 <- pure (CPtr p1)
            (CPtr p2) @ _1 <- pure n3
            (CPtr p3) @ _2 <- pure n3
            v1 <- fetch p2
            _3 <- update p2 n2
            v2 <- fetch p3
            pure ()
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [0]
    result `shouldBe` expected

  it "finds location inside shared node - realistic" $ do
    let code = [prog|
          grinMain =
            n1 <- pure (FFoo)
            p1 <- store n1
            n3 <- pure (CPtr p1)
            (CPtr p2) @ _0 <- pure n3
            (CPtr p3) @ _1 <- pure n3
            v1 <- eval1 p2
            v2 <- eval2 p3
            pure ()

          eval1 q.1 =
            r.1 <- fetch q.1
            case r.1 of
              (CTwo) @ alt1.1 ->
                pure r.1
              (FFoo) @ alt2.1 ->
                z.1 <- pure (CTwo)
                _2.1 <- update q.1 z.1
                pure z.1

          eval2 q.2 =
            r.2 <- fetch q.2
            case r.2 of
              (CTwo) @ alt1.2 ->
                pure r.2
              (FFoo) @ alt2.2 ->
                z.2 <- pure (CTwo)
                _2.2 <- update q.2 z.2
                pure z.2
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [0]
    result `shouldBe` expected

  it "finds locations shared via as-pattern aliases" $ do
    let code = [prog|
          grinMain =
            n1 <- pure (COne)
            n2 <- pure (CTwo)
            p1 <- store n1
            n3 <- pure (CPtr p1)
            (CPtr p2) @ n4 <- pure n3
            (CPtr p3) @ _1 <- pure n4
            v1 <- fetch p2
            _2 <- update p2 n2
            v2 <- fetch p3
            pure ()
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [0]
    result `shouldBe` expected

  it "shared case scrutinee" $ do
    let code = [prog|
          grinMain =
            n1 <- pure (COne)
            n2 <- pure (CTwo)
            p1 <- store n1
            n3 <- pure (CPtr p1)
            case n3 of
              (CPtr p2) @ n4 ->
                (CPtr p3) @ _1 <- pure n4
                v1 <- fetch p2
                _2 <- update p2 n2
                v2 <- fetch p3
                pure ()
        |]
    let result = calcSharedLocations code
    let expected = Set.fromList [0]
    result `shouldBe` expected

calcSharedLocations :: Exp -> Set Loc
calcSharedLocations = _sharedLocs . calcSharingResult

calcSharingResult :: Exp -> SharingResult
calcSharingResult prog
  | (shProgram, shMapping) <- codeGen prog
  , computer <- _airComp . evalAbstractProgram $ shProgram
  , shResult <- toSharingResult shMapping computer
  = shResult

testProgram :: Exp
-- The syntax conversion preserves the abstarct heap layout
testProgram = withPrimPrelude [prog|
  grinMain =
    y.0 <- pure 1
    v.0 <- pure (CInt y.0)
    t1  <- store v.0
    y.1 <- pure 10000
    v.1 <- pure (CInt y.1)
    t2  <- store v.1
    v.2 <- pure (Fupto t1 t2)
    t3  <- store v.2
    v.3 <- pure (Fsum t3)
    t4  <- store v.3
    (CInt r') @ p.0 <- eval $ t4
    _prim_int_print $ r'

  upto m n =
    (CInt m') @ p.2 <- eval $ m
    (CInt n') @ p.1 <- eval $ n
    b' <- _prim_int_gt $ m' n'
    case b' of
      #True @ alt.0 ->
        v.4 <- pure (CNil)
        pure v.4
      #False @ alt.1 ->
        x.7 <- pure 1
        m1' <- _prim_int_add $ m' x.7
        v.5 <- pure (CInt m1')
        m1  <- store v.5
        v.6 <- pure (Fupto m1 n)
        p   <- store v.6
        v.7 <- pure (CCons m p)
        pure v.7

  sum l =
    l2 <- eval $ l
    case l2 of
      (CNil) @ alt.2 ->
        y.10 <- pure 0
        v.8  <- pure (CInt y.10)
        pure v.8
      (CCons x xs) @ alt.3 ->
        (CInt x') @ p.4 <- eval $ x
        (CInt s') @ p.3 <- sum $ xs
        ax' <- _prim_int_add $ x' s'
        v.9 <- pure (CInt ax')
        pure v.9

  eval q =
    v <- fetch q
    case v of
      (CInt x'1) @ alt.4 ->
        pure alt.4
      (CNil) @ alt.5 ->
        pure alt.5
      (CCons y ys) @ alt.6 ->
        pure alt.6
      (Fupto a b) @ alt.7 ->
        w <- upto $ a b
        p.5 <- update q w
        pure w
      (Fsum c) @ alt.8 ->
        z <- sum $ c
        p.6 <- update q z
        pure z
  |]
