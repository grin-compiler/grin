{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.Pretty where

import Data.Int
import qualified Data.Bimap as Bimap

import Text.PrettyPrint.ANSI.Leijen

import Pretty ()
import Grin (Name)
import AbstractInterpretation.IR
import AbstractInterpretation.HPTResultNew (toLocValue)
import AbstractInterpretation.PrettyHPT ()

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
  pretty (Mem a) = green $ text "$" <> (integer $ fromIntegral a)

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
prettySimpleType = pretty . toLocValue

prettyReg :: Maybe HPTProgram -> Reg -> Doc
prettyReg mhpt reg@(Reg a) = regName <> (green $ text "@" <> (integer $ fromIntegral a))  where
  regName = maybe empty (\hpt -> maybe empty prettyName $ Bimap.lookupR reg $ hptRegisterMap hpt) mhpt

prettyTag :: Maybe HPTProgram -> Tag -> Doc
prettyTag mhpt tag@(Tag a) = parens (integer $ fromIntegral a) <> tagName where
  tagName = maybe empty (\hpt -> pretty $ hptTagMap hpt Bimap.!> tag) mhpt

prettySelector :: Maybe HPTProgram -> Selector -> Doc
prettySelector mhpt (NodeItem tag idx) = prettyTag mhpt tag <> brackets (pretty idx)

prettyCondition :: Maybe HPTProgram -> Condition -> Doc
prettyCondition mhpt = \case
    NodeTypeExists a    -> prettyTag mhpt a
    SimpleTypeExists a  -> prettySimpleType a <> braces (integer $ fromIntegral a)

prettyConstant :: Maybe HPTProgram -> Constant -> Doc
prettyConstant mhpt = \case
    CSimpleType a   -> ppS a
    CHeapLocation a -> pretty a
    CNodeType tag arity -> ppT tag <> angles (pretty arity)
    CNodeItem tag idx a -> ppT tag <> brackets (pretty idx) <> text "=" <> (
      if a < 0
        then ppS a
        else pretty . Mem $ fromIntegral a
      )
  where
    ppT = prettyTag mhpt
    ppS a = prettySimpleType a <> text "#" <> (integer $ fromIntegral a)

prettyInstruction :: Maybe HPTProgram -> Instruction -> Doc
prettyInstruction mhpt = \case
    If      {..} -> keyword "if" <+> prettyCondition mhpt condition <+> keyword "in" <+> ppR srcReg <$$> indent 2 (vsep . map (prettyInstruction mhpt) $ instructions)
    Project {..} -> keyword "project" <+> ppS srcSelector <+> ppR srcReg <+> ppR dstReg
    Extend  {..} -> keyword "extend" <+> ppR srcReg <+> ppS dstSelector <+> ppR dstReg
    Move    {..} -> keyword "move" <+> ppR srcReg <+> ppR dstReg
    Fetch   {..} -> keyword "fetch" <+> ppR addressReg <+> ppR dstReg
    Store   {..} -> keyword "store" <+> ppR srcReg <+> pretty address
    Update  {..} -> keyword "update" <+> ppR srcReg <+> ppR addressReg
    Set     {..} -> keyword "set" <+> ppR dstReg <+> prettyConstant mhpt constant
  where
    ppR = prettyReg mhpt
    ppS = prettySelector mhpt

prettyInstructions :: Maybe HPTProgram -> [Instruction] -> Doc
prettyInstructions mhpt = vsep . map (prettyInstruction mhpt)
