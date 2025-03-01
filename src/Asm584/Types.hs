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

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

-- | Register number (0-7).
type RFNumber = Int

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
  | NumberSign
  deriving (Eq, Show)
