module Assertions where

import Test.Hspec

import Grin
import Pretty

-- | Check if the two expression are the sam, if not renders them
-- in a pretty printed form.
sameAs :: Exp -> Exp -> IO ()
sameAs found expected = (PP found) `shouldBe` (PP expected)
