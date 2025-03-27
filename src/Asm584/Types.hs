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

module Asm584.Types where

import Data.Map (Map)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Word
import Text.Megaparsec hiding (label)

-- *** Assembly code representation *** --

-- | Symbolic label.
type Label = Text

-- | Instruction address in the range [0, 1023].
type Address = Int

-- | Register number in the range [0, 7].
type RFNumber = Int

-- | Whether a breakpoint is set or not.
type Breakpoint = Bool

-- | Value of ALUCIN (0 or 1).
type AlucinValue = Bool

-- | Character offset in the source code.
type SourceOffset = Int

data Program = Program
  { statements :: [Statement],
    labels :: Map Label Address
  }
  deriving (Eq, Show)

data Statement = Statement
  { label :: Maybe (Label, SourceOffset),
    breakpoint :: Breakpoint,
    instruction :: Instruction,
    alucinValue :: Maybe AlucinValue,
    controlStatement :: Maybe ControlStatement,
    comment :: Maybe Text
  }
  deriving (Eq, Show)

data Instruction
  = -- Group 1: arithmetic/logical instructions.
    RF_Assign_RF_Op_WR Operation RFNumber
  | WR_Assign_RF_Op_WR Operation RFNumber
  | DO_Assign_DI_Op_WR Operation
  | WR_Assign_DI_Op_WR Operation
  | WR_Assign_DI_Op_XWR Operation
  | XWR_Assign_DI_Op_WR Operation
  | XWR_Assign_DI_Op_XWR Operation
  | DO_Assign_DI_Op_XWR Operation
  | -- Group 2: addition instructions.
    XWR_Assign_RF_Plus_WR_Plus_ALUCIN RFNumber
  | WR_Assign_RF_Plus_DI_Plus_ALUCIN RFNumber
  | XWR_Assign_RF_Plus_DI_Plus_ALUCIN RFNumber
  | RF_Assign_RF_Plus_DI_Plus_ALUCIN RFNumber
  | WR_Assign_RF_Plus_XWR_Plus_ALUCIN RFNumber
  | XWR_Assign_RF_Plus_XWR_Plus_ALUCIN RFNumber
  | RF_Assign_XWR_Plus_ALUCIN RFNumber
  | XWR_Assign_WR_Plus_DI_Plus_ALUCIN
  | DO_Assign_WR_Plus_DI_Plus_ALUCIN
  | WR_Assign_XWR_Plus_DI_Plus_ALUCIN
  | XWR_Assign_XWR_Plus_DI_Plus_ALUCIN
  | DO_Assign_XWR_Plus_ALUCIN
  | -- Group 3: move instructions.
    RF_Assign_DI RFNumber
  | DO_Assign_RF RFNumber
  | XWR_Assign_RF RFNumber
  | WR_Assign_DI
  | XWR_Assign_DI
  | DO_Assign_DI
  | -- Group 4: double-precision shift/arithmetic instructions.
    WRXWR_Assign_RSL_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN
  | WRXWR_Assign_RSL_WRXWR_Plus_DI_Plus_ALUCIN
  | WRXWR_Assign_RSL_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN RFNumber
  | WRXWR_Assign_RSL_WRXWR_Plus_RF_Plus_ALUCIN RFNumber
  | WRXWR_Assign_ASR_WRXWR_Plus_ALUCIN
  | WRXWR_Assign_ASR_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN
  | WRXWR_Assign_ASR_WRXWR_Plus_DI_Plus_ALUCIN
  | WRXWR_Assign_ASR_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN RFNumber
  | WRXWR_Assign_ASR_WRXWR_Plus_RF_Plus_ALUCIN RFNumber
  | -- Group 5: single-precision shift instructions.
    WR_Assign_ASR_WR_Plus_ALUCIN
  | WR_Assign_RSR_WR_Plus_ALUCIN
  | WR_Assign_ASL_WR_Plus_ALUCIN
  | WR_Assign_RSL_WR_Plus_ALUCIN
  | WR_Assign_LSR_WR_Plus_ALUCIN
  | WR_Assign_LSL_WR_Plus_ALUCIN
  | -- Group 6: double-precision shift instructions.
    WRXWR_Assign_RSR_WRXWR_Plus_ALUCIN
  | WRXWR_Assign_ASL_WRXWR_Plus_ALUCIN
  | WRXWR_Assign_RSL_WRXWR_Plus_ALUCIN
  | WRXWR_Assign_LSR_WRXWR_Plus_ALUCIN
  | WRXWR_Assign_LSL_WRXWR_Plus_ALUCIN
  | -- Special instructions.
    No_Operation
  deriving (Eq, Ord, Show)

data Operation
  = -- Arithmetic operations.
    Not_ALUCIN
  | B_Minus_A_Minus_One_Plus_ALUCIN
  | A_Minus_B_Minus_One_Plus_ALUCIN
  | A_Plus_B_Plus_ALUCIN
  | B_Plus_ALUCIN
  | Not_B_Plus_ALUCIN
  | A_Plus_ALUCIN
  | Not_A_Plus_ALUCIN
  | -- Logical operations.
    A_And_B
  | A_Xor_B
  | A_Xnor_B
  | Not_A_And_B
  | A_And_Not_B
  | A_Or_Not_B
  | Not_A_Or_B
  | A_Or_B
  deriving (Enum, Eq, Ord, Show)

data Alucin
  = -- | Instruction does not use ALUCIN.
    NoAlucin
  | -- | Instruction requires a value of ALUCIN after the mnemonic.
    NeedsAlucin
  deriving (Eq, Show)

data ControlStatement
  = ControlStatement_If
      Condition
      (Location, SourceOffset)
      (Maybe (Location, SourceOffset))
  | ControlStatement_Goto (Location, SourceOffset)
  | ControlStatement_Input Word16
  deriving (Eq, Show)

data Condition
  = Condition_ALUCOUT
  | Condition_ALUCOUT0
  | Condition_ALUCOUT1
  | Condition_ALUCOUT2
  | Condition_Not_WRRT
  | Condition_Not_WRLFT
  | Condition_Not_XWRRT
  | Condition_Not_XWRLFT
  | Condition_XWR0
  | Condition_XWR3
  | Condition_AMSB
  | Condition_BMSB
  deriving (Eq, Show)

data Location
  = Location_Label Label
  | Location_Address Address
  deriving (Eq, Show)

-- *** Parser *** --

type Parser = Parsec ParserError Text

-- *** Tokens *** --

type TokenSequence = [Tok]

data Tok
  = -- Registers.
    WR
  | XWR
  | RF RFNumber
  | -- Input/output ports.
    DI
  | DO
  | -- Inputs.
    ALUCIN
  | -- Outputs.
    ALUCOUT
  | ALUCOUT0
  | ALUCOUT1
  | ALUCOUT2
  | WRRT
  | WRLFT
  | XWRRT
  | XWRLFT
  | XWR0
  | XWR3
  | AMSB
  | BMSB
  | -- Arithmetic operators.
    Plus
  | Minus
  | Zero
  | One
  | -- Logical operators.
    And
  | Or
  | Xor
  | Not
  | -- Shift operators.
    LSR
  | LSL
  | ASR
  | ASL
  | RSR
  | RSL
  | -- Special keywords.
    NOP
  | Break
  | -- Control flow statements.
    If
  | Then
  | Else
  | Goto
  | Input
  | -- Misc.
    Assign
  | Equal
  | OpenParen
  | CloseParen
  | Comma
  | Colon
  deriving (Eq, Ord, Show)

-- *** Errors *** --

data ParserError
  = AddressIsOutOfRange Integer
  | InputValueIsOutOfRange Integer
  | LabelIsAlreadyDefined Label
  | LabelIsNotFound Label
  | TooManyInstructions
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParserError where
  showErrorComponent (AddressIsOutOfRange address) =
    [i|address #{address} is out of range [0, #{maxInstructionCount - 1}]|]
  showErrorComponent (InputValueIsOutOfRange value) =
    [i|input value #{value} is out of range [-32768, 65535]|]
  showErrorComponent (LabelIsAlreadyDefined label) =
    [i|label "#{label}" is already defined|]
  showErrorComponent (LabelIsNotFound label) =
    [i|label "#{label}" is not found|]
  showErrorComponent TooManyInstructions =
    [i|program cannot have more than #{maxInstructionCount} microinstructions|]

-- *** Constants *** --

maxInstructionCount :: Int
maxInstructionCount = 1024
