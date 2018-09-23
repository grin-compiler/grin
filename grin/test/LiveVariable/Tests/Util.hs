module LiveVariable.Tests.Util
  ( module LiveVariable.Tests.Util
  , module Test.Util
  ) where

import Data.Map    (Map)
import Data.Vector (Vector)

import qualified Data.Map    as M
import qualified Data.Vector as V

import System.FilePath

import Grin.Grin

import Test.Util

import AbstractInterpretation.LVAResult hiding (node)



lvaExamples :: FilePath
lvaExamples = "LiveVariable" </> "examples"

live :: Bool
live = True

dead :: Bool
dead = False

liveVal :: Liveness
liveVal = BasicVal True

deadVal :: Liveness
deadVal = BasicVal False

liveLoc :: Liveness
liveLoc = BasicVal True

deadLoc :: Liveness
deadLoc = BasicVal False

node :: [Bool] -> Node
node = Node . V.fromList

nodeSet :: [(Tag,[Bool])] -> Liveness
nodeSet = NodeSet . M.fromList . map (\(t,fs) -> (t, node fs))

fun :: (Liveness, [Liveness]) -> (Liveness, Vector Liveness)
fun = fmap V.fromList

mkFunctionLivenessMap :: [(Name, (Liveness, Vector Liveness))]
                      -> Map Name (Liveness, Vector Liveness)
mkFunctionLivenessMap = M.insert "grinMain" (fun (liveVal,[])) . M.fromList
