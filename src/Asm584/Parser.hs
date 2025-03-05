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
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char

-- *** Parsers *** --

-- | Parses a label (an identifier followed by colon character).
-- Properly disambiguates labels from destination operands followed by
-- Pascal-style assignment, e.g. "RF0" is not interpreted as label in the input
-- string "RF0:=DI".
labelP :: Parser Label
labelP = (identifierP <?> "label") <* notFollowedBy assignP <* colonP

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

controlStatementP :: Parser ControlStatement
controlStatementP =
  choice
    [ ControlStatement_If
        <$ ifP
        <*> conditionP
        <* thenP
        <*> locationP
        <*> optional (elseP *> locationP),
      ControlStatement_Goto <$ gotoP <*> locationP,
      ControlStatement_Input <$ inputP <*> inputValueP
    ]

conditionP :: Parser Condition
conditionP =
  choice'
    [ Condition_ALUCOUT <$ alucoutP,
      Condition_ALUCOUT0 <$ alucout0P,
      Condition_ALUCOUT1 <$ alucout1P,
      Condition_ALUCOUT2 <$ alucout2P,
      Condition_Not_WRRT <$ notP <* wrrtP,
      Condition_Not_WRLFT <$ notP <* wrlftP,
      Condition_Not_XWRRT <$ notP <* xwrrtP,
      Condition_Not_XWRLFT <$ notP <* xwrlftP,
      Condition_XWR0 <$ xwr0P,
      Condition_XWR3 <$ xwr3P,
      Condition_AMSB <$ amsbP,
      Condition_BMSB <$ bmsbP
    ]

locationP :: Parser Location
locationP =
  (Location_Label <$> identifierP <?> "label")
    <|> (Location_Address <$> addressP <?> "address")
  where
    addressP = fromInteger <$> valueInRange (0, maxInstructions - 1) decimal

-- | Parses a 16-bit input value in one of the following formats:
-- 1. A 16-digit binary number.
-- 2. A binary number divided into 4 groups of 4 digits separated by space.
-- 3. A hexadecimal number.
-- 4. A signed or unsigned decimal number.
inputValueP :: Parser Word16
inputValueP =
  choice
    [ try $ fromInteger <$> binaryN 16,
      try $ fromInteger <$> binaryMxN 4 4 spaceChar,
      fromInteger <$> valueInRange (0, 65535) hexadecimal,
      fromInteger <$> valueInRange (-32768, 65535) (signed decimal)
    ]

-- *** Utilities *** --

maxInstructions :: (Num a) => a
maxInstructions = 1024

parens :: Parser a -> Parser a
parens = between openParenP closeParenP

-- | Backtracking version of 'choice'. If the current parser fails, it rolls
-- back the parser state before trying the next alternative.
choice' :: [Parser a] -> Parser a
choice' = choice . map try

valueInRange :: (Ord a, Show a) => (a, a) -> Parser a -> Parser a
valueInRange (min', max') p = do
  value <- p
  if value >= min' && value <= max'
    then pure value
    else fail $ "value is out of range " ++ show min' ++ "-" ++ show max'
