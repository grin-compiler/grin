{-# LANGUAGE TemplateHaskell #-}
module Grin.ExtendedSyntax.TH
  ( text
  , progConst
  , prog
  , def
  , expr
  ) where

import Data.List (sort)
import Data.Char
import Data.Data
import Data.Maybe
import NeatInterpolation
import Text.Megaparsec

import qualified Grin.ExtendedSyntax.Parse as P
import qualified Data.Text as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

prog :: QuasiQuoter
prog = text { quoteExp = applyParseProg . quoteExp text }

applyParseProg:: Q Exp -> Q Exp
applyParseProg q = appE [|P.parseProg|] q

def :: QuasiQuoter
def = text { quoteExp = applyParseDef . quoteExp text }

applyParseDef :: Q Exp -> Q Exp
applyParseDef q = appE [|P.parseDef|] q

expr :: QuasiQuoter
expr = text { quoteExp = applyParseExpr . quoteExp text }

applyParseExpr :: Q Exp -> Q Exp
applyParseExpr q = appE [|P.parseExpr|] q

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)

-- NOTE: does not support metavariables
progConst :: QuasiQuoter
progConst = QuasiQuoter
  { quoteExp = \input -> do
      let src = T.pack $ normalizeQQInput input
      case P.parseGrin "" src of
        Left  e -> fail $ errorBundlePretty e
        Right p -> liftDataWithText p
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

--
-- NOTE: copy-paste utility from NeatInterpolation.String hidden module
--
normalizeQQInput :: [Char] -> [Char]
normalizeQQInput = trim . unindent' . tabsToSpaces
  where
    unindent' :: [Char] -> [Char]
    unindent' s =
      case lines s of
        head:tail ->
          let
            unindentedHead = dropWhile (== ' ') head
            minimumTailIndent = minimumIndent . unlines $ tail
            unindentedTail = case minimumTailIndent of
              Just indent -> map (drop indent) tail
              Nothing -> tail
          in unlines $ unindentedHead : unindentedTail
        [] -> []

trim :: [Char] -> [Char]
trim = dropWhileRev isSpace . dropWhile isSpace

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

unindent :: [Char] -> [Char]
unindent s =
  case minimumIndent s of
    Just indent -> unlines . map (drop indent) . lines $ s
    Nothing -> s

tabsToSpaces :: [Char] -> [Char]
tabsToSpaces ('\t':tail) = "    " ++ tabsToSpaces tail
tabsToSpaces (head:tail) = head : tabsToSpaces tail
tabsToSpaces [] = []

minimumIndent :: [Char] -> Maybe Int
minimumIndent =
  listToMaybe . sort . map lineIndent
    . filter (not . null . dropWhile isSpace) . lines

-- | Amount of preceding spaces on first line
lineIndent :: [Char] -> Int
lineIndent = length . takeWhile (== ' ')
