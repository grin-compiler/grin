{-# LANGUAGE TypeApplications, OverloadedStrings #-}
module Transformations.Optimising.CaseCopyPropagationSpec where

import Transformations.Optimising.CaseCopyPropagation

import Test.Hspec
import Free
import Grin
import Test
import Assertions


spec :: Spec
spec = do

  it "Example from Figure 4.26" $ do
    before <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: block (switch "v"
        [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                             unit @Val ("Int" @: ["y'"]))
        , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                             unit @Val ("Int" @: ["z'"]))
        , ("Int" @: ["x'"], unit @Val ("Int" @: ["x'"]))
        ]) $
      unit @Var "m0"

    after  <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: (block $
                "v'" <=: (block $ switch "v"
                  [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                                       unit @Var "y'")
                  , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                                       unit @Var "z'")
                  , ("Int" @: ["x'"], unit @Var "x'")
                  ]) $
                unit @Val ("Int" @: ["v'"])) $
      unit @Var "m0"

    caseCopyPropagation before `sameAs` after

  it "One node has no Int tagged value" $ do
    before <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: block (switch "v"
        [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                             unit @Val ("Int" @: ["y'"]))
        , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                             unit @Val ("Float" @: ["z'"]))
        , ("Int" @: ["x'"], unit @Val ("Int" @: ["x'"]))
        ]) $
      unit @Var "m0"

    after <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: block (switch "v"
        [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                             unit @Val ("Int" @: ["y'"]))
        , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                             unit @Val ("Float" @: ["z'"]))
        , ("Int" @: ["x'"], unit @Val ("Int" @: ["x'"]))
        ]) $
      unit @Var "m0"

    caseCopyPropagation before `sameAs` after

  it "Embedded good case" $ do
    before <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: block (switch "v"
        [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                             unit @Val ("Int" @: ["y'"]))
        , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                             unit @Val ("Int" @: ["z'"]))
        , ("Int" @: ["x'"], "u1" <=: block (switch "v1"
                                [ ("foo" @@: ["a1"],  "y1'" <=: app "foo" ["a1"] $
                                                      unit @Val ("Int" @: ["y1'"]))
                                , ("bar" @@: ["b1"],  "z1'" <=: app "bar" ["b1"] $
                                                      unit @Val ("Int" @: ["z1'"]))
                                , ("Int" @: ["x1'"], unit @Val ("Int" @: ["x1'"]))
                                ]) $
                            unit @Val ("Int" @: ["x'"]))
        ]) $
      unit @Var "m0"

    after  <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: (block $
                "v'" <=: (block $ switch "v"
                  [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                                       unit @Var "y'")
                  , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                                       unit @Var "z'")
                  , ("Int" @: ["x'"], "u1" <=: (block $
                                                "v1'" <=: (block $ switch "v1"
                                                  [ ("foo" @@: ["a1"],  "y1'" <=: app "foo" ["a1"] $
                                                                        unit @Var "y1'")
                                                  , ("bar" @@: ["b1"],  "z1'" <=: app "bar" ["b1"] $
                                                                        unit @Var "z1'")
                                                  , ("Int" @: ["x1'"],  unit @Var "x1'")
                                                  ]) $
                                                unit @Val ("Int" @: ["v1'"])) $
                                      unit @Var "x'")
                  ]) $
                unit @Val ("Int" @: ["v'"])) $
      unit @Var "m0"

    caseCopyPropagation before `sameAs` after

  it "Embedded bad case" $ do
    before <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: block (switch "v"
        [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                             unit @Val ("Int" @: ["y'"]))
        , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                             unit @Val ("Int" @: ["z'"]))
        , ("Int" @: ["x'"], "u1" <=: block (switch "v1"
                                [ ("foo" @@: ["a1"],  "y1'" <=: app "foo" ["a1"] $
                                                      unit @Val ("Int" @: ["y1'"]))
                                , ("bar" @@: ["b1"],  "z1'" <=: app "bar" ["b1"] $
                                                      unit @Val ("Float" @: ["z1'"]))
                                , ("Int" @: ["x1'"], unit @Val ("Int" @: ["x1'"]))
                                ]) $
                            unit @Val ("Int" @: ["x'"]))
        ]) $
      unit @Var "m0"

    after  <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: (block $
                "v'" <=: (block $ switch "v"
                  [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                                       unit @Var "y'")
                  , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                                       unit @Var "z'")
                  , ("Int" @: ["x'"], "u1" <=: block (switch "v1"
                                        [ ("foo" @@: ["a1"],  "y1'" <=: app "foo" ["a1"] $
                                                              unit @Val ("Int" @: ["y1'"]))
                                        , ("bar" @@: ["b1"],  "z1'" <=: app "bar" ["b1"] $
                                                              unit @Val ("Float" @: ["z1'"]))
                                        , ("Int" @: ["x1'"], unit @Val ("Int" @: ["x1'"]))
                                        ]) $
                                      unit @Var "x'")
                  ]) $
                unit @Val ("Int" @: ["v'"])) $
      unit @Var "m0"

    caseCopyPropagation before `sameAs` after

  it "Leave the outher transform the inner" $ do
    before <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: block (switch "v"
        [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                             unit @Val ("Int" @: ["y'"]))
        , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                             unit @Val ("Float" @: ["z'"]))
        , ("Int" @: ["x'"], "u1" <=: block (switch "v1"
                                [ ("foo" @@: ["a1"],  "y1'" <=: app "foo" ["a1"] $
                                                      unit @Val ("Int" @: ["y1'"]))
                                , ("bar" @@: ["b1"],  "z1'" <=: app "bar" ["b1"] $
                                                      unit @Val ("Int" @: ["z1'"]))
                                , ("Int" @: ["x1'"], unit @Val ("Int" @: ["x1'"]))
                                ]) $
                            unit @Val ("Int" @: ["x'"]))
        ]) $
      unit @Var "m0"

    after <- buildExpM $
      "m0" <=: store @Int 3 $
      "u" <=: block (switch "v"
        [ ("foo" @@: ["a"],  "y'" <=: app "foo" ["a"] $
                             unit @Val ("Int" @: ["y'"]))
        , ("bar" @@: ["b"],  "z'" <=: app "bar" ["b"] $
                             unit @Val ("Float" @: ["z'"]))
                  , ("Int" @: ["x'"], "u1" <=: (block $
                                                "v1'" <=: (block $ switch "v1"
                                                  [ ("foo" @@: ["a1"],  "y1'" <=: app "foo" ["a1"] $
                                                                        unit @Var "y1'")
                                                  , ("bar" @@: ["b1"],  "z1'" <=: app "bar" ["b1"] $
                                                                        unit @Var "z1'")
                                                  , ("Int" @: ["x1'"],  unit @Var "x1'")
                                                  ]) $
                                                unit @Val ("Int" @: ["v1'"])) $
                                      unit @Val ("Int" @: ["x'"]))
        ]) $
      unit @Var "m0"

    caseCopyPropagation before `sameAs` after

runTests :: IO ()
runTests = hspec spec
