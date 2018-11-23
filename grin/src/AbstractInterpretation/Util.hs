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
  p { _absInstructions = reverseInstructions _absInstructions }

converge :: (a -> a -> Bool) -> (a -> a) -> a -> a
converge pred f x
  | pred x x' = x
  | otherwise = converge pred f x'
  where x' = f x