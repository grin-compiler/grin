{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.Pretty where

import Text.PrettyPrint.ANSI.Leijen

import AbstractInterpretation.IR

printHptIr :: [Instruction] -> IO ()
printHptIr = putDoc . pretty

keyword :: String -> Doc
keyword = yellow . text

instance Pretty Instruction where
  prettyList = vsep . map pretty
  pretty = \case
    If      {..} -> keyword "if" <+> pretty condition <+> keyword "in" <+> pretty srcReg <$$> indent 2 (pretty instructions)
    Project {..} -> keyword "project" <+> pretty srcSelector <+> pretty srcReg <+> pretty dstReg
    Extend  {..} -> keyword "extend" <+> pretty srcReg <+> pretty dstSelector <+> pretty dstReg
    Move    {..} -> keyword "move" <+> pretty srcReg <+> pretty dstReg
    Fetch   {..} -> keyword "fetch" <+> pretty addressReg <+> pretty dstReg
    Store   {..} -> keyword "store" <+> pretty srcReg <+> pretty address
    Update  {..} -> keyword "update" <+> pretty srcReg <+> pretty addressReg
    Init    {..} -> keyword "init" <+> pretty dstReg <+> pretty constant

instance Pretty Reg where
  pretty (Reg a) = green $ text "@" <> (integer $ fromIntegral a)

instance Pretty Mem where
  pretty (Mem a) = green $ text "$" <> (integer $ fromIntegral a)

instance Pretty Tag where
  pretty (Tag a) = parens $ (integer $ fromIntegral a)

instance Pretty Selector where
  pretty (NodeItem tag idx) = pretty tag <> brackets (pretty idx)

instance Pretty Condition where
  pretty = \case
    NodeTypeExists a    -> pretty a
    SimpleTypeExists a  -> pretty (integer $ fromIntegral a)

instance Pretty Constant where
  pretty _ = text "" -- TODO

{-
  = CSimpleType   SimpleType
  | CHeapLocation Mem
  | CNodeType     Tag Int { -arity- }
  | CNodeItem     Tag Int { -node item index- } Int32 { -simple type or location- }
-}
