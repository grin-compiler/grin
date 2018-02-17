{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.Pretty where

import Data.Int
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.PrettyPrint.ANSI.Leijen

import Pretty ()
import Grin (Name)
import qualified Grin
import AbstractInterpretation.IR
import AbstractInterpretation.HPTResultNew (toSimpleType)
import AbstractInterpretation.PrettyHPT ()

data IRMap
  = IRMap
  { irmRegisterMap  :: Map.Map Reg (Set.Set Name)
  , irmTagMap       :: Bimap.Bimap Grin.Tag Tag
  }

printHptIr :: [Instruction] -> IO ()
printHptIr = putDoc . pretty

keyword :: String -> Doc
keyword = yellow . text

{-
  show
    done - tag name
    done - register name
    done - simple type
    function argument
    function return type

  custom printer
    done - Tag
    done - Reg
    done - Instruction
    done - Selector
    done - Condition
    done - Constant
-}

instance Pretty Instruction where
  pretty = prettyInstruction Nothing

instance Pretty Reg where
  pretty = prettyReg Nothing

instance Pretty Mem where
  pretty (Mem a) = cyan $ text "$" <> (integer $ fromIntegral a)

instance Pretty Tag where
  pretty = prettyTag Nothing

instance Pretty Selector where
  pretty = prettySelector Nothing

instance Pretty Condition where
  pretty = prettyCondition Nothing

instance Pretty Constant where
  pretty = prettyConstant Nothing

prettyName :: Name -> Doc
prettyName = red . text

prettySimpleType :: Int32 -> Doc
prettySimpleType = pretty . toSimpleType

prettyReg :: Maybe IRMap -> Reg -> Doc
prettyReg mirm reg@(Reg a) = regName <> (green $ text "@" <> (integer $ fromIntegral a)) where
  regName = maybe empty (\irm -> maybe empty (encloseSep lbrace rbrace comma . map prettyName . Set.toList) $ Map.lookup reg $ irmRegisterMap irm) mirm

prettyTag :: Maybe IRMap -> Tag -> Doc
prettyTag mirm tag@(Tag a) = parens (integer $ fromIntegral a) <> tagName where
  tagName = maybe empty (\irm -> pretty $ irmTagMap irm Bimap.!> tag) mirm

prettySelector :: Maybe IRMap -> Selector -> Doc
prettySelector mirm (NodeItem tag idx) = prettyTag mirm tag <> brackets (pretty idx)

prettyCondition :: Maybe IRMap -> Condition -> Doc
prettyCondition mirm = \case
    NodeTypeExists a    -> prettyTag mirm a
    SimpleTypeExists a  -> prettySimpleType a <> text "#" <> (integer $ fromIntegral a)

prettyConstant :: Maybe IRMap -> Constant -> Doc
prettyConstant mirm = \case
    CSimpleType a   -> ppS a
    CHeapLocation a -> pretty a
    CNodeType tag arity -> ppT tag <> angles (pretty arity)
    CNodeItem tag idx a -> ppT tag <> brackets (pretty idx) <> text "=" <> (
      if a < 0
        then ppS a
        else pretty . Mem $ fromIntegral a
      )
  where
    ppT = prettyTag mirm
    ppS a = prettySimpleType a <> text "#" <> (integer $ fromIntegral a)

prettyInstruction :: Maybe IRMap -> Instruction -> Doc
prettyInstruction mirm = \case
    If      {..} -> keyword "if" <+> prettyCondition mirm condition <+> keyword "in" <+> ppR srcReg <$$> indent 2 (vsep . map (prettyInstruction mirm) $ instructions)
    Project {..} -> keyword "project" <+> ppS srcSelector <+> ppR srcReg <+> arr <+> ppR dstReg
    Extend  {..} -> keyword "extend" <+> ppR srcReg <+> ppS dstSelector <+> arr <+> ppR dstReg
    Move    {..} -> keyword "move" <+> ppR srcReg <+> arr <+> ppR dstReg
    Fetch   {..} -> keyword "fetch" <+> ppR addressReg <+> arr <+> ppR dstReg
    Store   {..} -> keyword "store" <+> ppR srcReg <+> arr <+> pretty address
    Update  {..} -> keyword "update" <+> ppR srcReg <+> arr <+> ppR addressReg
    Set     {..} -> keyword "set" <+> prettyConstant mirm constant <+> arr <+> ppR dstReg
  where
    ppR = prettyReg mirm
    ppS = prettySelector mirm
    arr = text "-->"

prettyInstructions :: Maybe HPTProgram -> [Instruction] -> Doc
prettyInstructions mhpt = vsep . map (prettyInstruction mirm) where
  mirm = fmap toIRMap mhpt
  toIRMap hpt = IRMap
    { irmRegisterMap  = Map.unionsWith mappend [Map.singleton reg (Set.singleton name) | (name,reg) <- Map.toList $ hptRegisterMap hpt]
    , irmTagMap       = hptTagMap hpt
    }
