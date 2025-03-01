{-
 - Copyright (C) 2025 Arthur Kasimov
 -
 - This file is part of asm584.
 -
 - asm584 is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Lesser General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - asm584 is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 - GNU Lesser General Public License for more details.
 -
 - You should have received a copy of the GNU Lesser General Public License
 - along with asm584. If not, see <https://www.gnu.org/licenses/>.
 -}

module Asm584.Parser where

import Asm584.Lexer
import Asm584.Types
import Data.Functor
import Text.Megaparsec

-- *** Parsers *** --

instructionP :: Parser Instruction
instructionP =
  choice'
    [ -- Group 1: arithmetic/logical instructions.
      do
        RF n <- rfP
        _ <- assignP
        op <- operationP (rfWithNumberP n) wrP
        pure $ RF_Assign_RF_Op_WR op,
      WR_Assign_RF_Op_WR <$ wrP <* assignP <*> operationP rfP wrP,
      DO_Assign_DI_Op_WR <$ doP <* assignP <*> operationP diP wrP,
      WR_Assign_DI_Op_WR <$ wrP <* assignP <*> operationP diP wrP,
      WR_Assign_DI_Op_XWR <$ wrP <* assignP <*> operationP diP xwrP,
      XWR_Assign_DI_Op_WR <$ xwrP <* assignP <*> operationP diP wrP,
      XWR_Assign_DI_Op_XWR <$ xwrP <* assignP <*> operationP diP xwrP,
      DO_Assign_DI_Op_XWR <$ doP <* assignP <*> operationP diP xwrP
    ]

operationP :: Parser Tok -> Parser Tok -> Parser Operation
operationP a b =
  choice'
    [ -- Arithmetic operations.
      Not_ALUCIN <$ notP <* alucinP <*> alucinValueP,
      flip B_Minus_A_Minus_One_Plus_ALUCIN
        <$> b
        <* minusP
        <*> a
        <* minusP
        <* oneP
        <* plusP
        <* alucinP
        <*> alucinValueP,
      A_Minus_B_Minus_One_Plus_ALUCIN
        <$> a
        <* minusP
        <*> b
        <* minusP
        <* oneP
        <* plusP
        <* alucinP
        <*> alucinValueP,
      A_Plus_B_Plus_ALUCIN
        <$> a
        <* plusP
        <*> b
        <* plusP
        <* alucinP
        <*> alucinValueP,
      B_Plus_ALUCIN <$> b <* plusP <* alucinP <*> alucinValueP,
      Not_B_Plus_ALUCIN <$ notP <*> b <* plusP <* alucinP <*> alucinValueP,
      A_Plus_ALUCIN <$> a <* plusP <* alucinP <*> alucinValueP,
      Not_A_Plus_ALUCIN <$ notP <*> a <* plusP <* alucinP <*> alucinValueP,
      -- Logical operations.
      A_And_B <$> a <* andP <*> b,
      A_Xor_B <$> a <* xorP <*> b,
      notP *> parens (A_Xnor_B <$> a <* xorP <*> b),
      Not_A_And_B <$ notP <*> a <* andP <*> b,
      A_And_Not_B <$> a <* andP <* notP <*> b,
      A_Or_Not_B <$> a <* orP <* notP <*> b,
      Not_A_Or_B <$ notP <*> a <* orP <*> b,
      A_Or_B <$> a <* orP <*> b
    ]

alucinValueP :: Parser ALUCINValue
alucinValueP = parens (alucinP *> equalP *> (zeroP <|> oneP)) <&> (== One)

-- *** Utilities *** --

parens :: Parser a -> Parser a
parens = between openParenP closeParenP

-- | Backtracking version of 'choice'. If the current parser fails, it rolls
-- back the parser state before trying the next alternative.
choice' :: [Parser a] -> Parser a
choice' = choice . map try
