{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.Util where

import Data.Functor.Foldable

import AbstractInterpretation.IR

reverseInstructions :: [Instruction] -> [Instruction]
reverseInstructions = reverse . map (cata alg)
  where
    alg :: InstructionF Instruction -> Instruction
    alg = \case
      IfF cond reg xs -> If cond reg (reverse xs)
      instructionF    -> embed instructionF

reverseProgram :: HasDataFlowInfo s => s -> s
reverseProgram = modifyInfo $ \p@AbstractProgram{..} ->
  p { absInstructions = reverseInstructions absInstructions }
