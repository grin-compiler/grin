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

-- Nodes with tag liveness info
node' :: [Bool] -> Node
node' [] = error "Please provide tag liveness"
node' (tagLv:fieldsLv) = Node tagLv (V.fromList fieldsLv)

-- Node sets with tag liveness info
nodeSet' :: [(Tag,[Bool])] -> Liveness
nodeSet' = NodeSet . M.fromList . map (\(t,fs) -> (t, node' fs))

-- Node with dead tag and dead fields
deadNode :: Int -> Node 
deadNode = Node <$> const False <*> flip V.replicate False 

-- Node set with dead tags and dead fields
deadNodeSet :: [(Tag,Int)] -> Liveness
deadNodeSet = NodeSet . M.fromList . map (\(t,n) -> (t, deadNode n))

-- Node with live tag
node :: [Bool] -> Node
node = Node <$> const True <*> V.fromList

-- Node set with live tags
nodeSet :: [(Tag,[Bool])] -> Liveness
nodeSet = NodeSet . M.fromList . map (\(t,fs) -> (t, node fs))

fun :: (Liveness, [Liveness]) -> (Liveness, Vector Liveness)
fun = fmap V.fromList

mkFunctionLivenessMap :: [(Name, (Liveness, Vector Liveness))]
                      -> Map Name (Liveness, Vector Liveness)
mkFunctionLivenessMap = M.insert "grinMain" (fun (liveVal,[])) . M.fromList
