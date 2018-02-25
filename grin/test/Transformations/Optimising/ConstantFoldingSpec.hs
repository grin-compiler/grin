{-# LANGUAGE TypeApplications, OverloadedStrings, LambdaCase #-}
module Transformations.Optimising.ConstantFoldingSpec where

import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Transformations.Optimising.ConstantFolding
import Check
import Free
import Grin
import Test
import Assertions


spec :: Spec
spec = do
  describe "constant folding" $ do
    it "inside bind" $ do
      x <- buildExpM $
        "x"  <=: store @Var "a"    $
        "y"  <=: store @Var "b"    $
        "u"  <=: unit  @Int 5      $
        Unit <=: store @Var "u"    $
        unit @Var "u"

      e <- buildExpM $
        "x"  <=: store @Var "a" $
        "y"  <=: store @Var "b" $
        Unit <=: store @Int 5   $
        unit @Int 5
      constantFolding x `sameAs` e

    it "last bind" $ do
      x <- buildExpM $
        "x" <=: store @Var "a" $
        "y" <=: store @Var "b" $
        "u" <=: unit  @Int 5   $
        unit @Var "u"
      e <- buildExpM $
        "x" <=: store @Var "a" $
        "y" <=: store @Var "b" $
        unit @Int 5
      constantFolding x `sameAs` e

    it "unused variable" $ do
      x <- buildExpM $
        "x" <=: store @Int 3 $
        "u" <=: unit  @Int 4 $
        unit @Int 5
      e <- buildExpM $
        "x" <=: store @Int 3 $
        unit @Int 5
      constantFolding x `sameAs` e

    it "only one statement" $ do
      x <- buildExpM $
        def "fun" ["a", "b"] $
          "x" <=: unit @Int 3 $
          unit @Var "x"
      e <- buildExpM $
        def "fun" ["a", "b"] $
          unit @Int 3
      constantFolding x `sameAs` e

  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "the program size shrinks" $ property $ forAll gen $ \original ->
        let transformed = constantFolding original
        in changed original transformed $ conjoin
            [ transformed `smallerThan` original
            , checkUniqueNames transformed
            ]

-- Check if the number of nodes in a program is less rhan or equals after the transformation.
smallerThan :: Exp -> Exp -> Property
smallerThan transformed original  =
  let sizeReduced  = programSize transformed
      sizeOriginal = programSize original
  in
    cover (sizeReduced == sizeOriginal) 0 "Non Reduced" $
    cover (sizeReduced <  sizeOriginal) 1 "Reduced"     $
    (sizeReduced <= sizeOriginal)

checkUniqueNames :: Exp -> Property
checkUniqueNames = label "Unique name" . null . nonUniqueNames

runTests :: IO ()
runTests = hspec spec
