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

module Asm584.CodeGen where

import Asm584.Types
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

encodeInstruction :: Instruction -> Maybe AlucinValue -> Breakpoint -> Word16
encodeInstruction instruction alucinValue breakpoint =
  toOpcode instruction
    .|. toAlucinAttrs alucinValue
    .|. toBreakpointAttr breakpoint

toOpcode :: Instruction -> Word16
-- Group 1: arithmetic/logical instructions.
toOpcode (RF_Assign_RF_Op_WR op n) =
  0b0000_00_000 .|. toOpField op .|. toSField n
toOpcode (WR_Assign_RF_Op_WR op n) =
  0b0000_01_000 .|. toOpField op .|. toSField n
toOpcode (DO_Assign_DI_Op_WR op) = 0b0000_11_000 .|. toOpField op
toOpcode (WR_Assign_DI_Op_WR op) = 0b0000_11_001 .|. toOpField op
toOpcode (WR_Assign_DI_Op_XWR op) = 0b0000_11_011 .|. toOpField op
toOpcode (XWR_Assign_DI_Op_WR op) = 0b0000_11_100 .|. toOpField op
toOpcode (XWR_Assign_DI_Op_XWR op) = 0b0000_11_110 .|. toOpField op
toOpcode (DO_Assign_DI_Op_XWR op) = 0b0000_11_111 .|. toOpField op
-- Group 2: addition instructions.
toOpcode (XWR_Assign_RF_Plus_WR_Plus_ALUCIN n) = 0b0011_10_000 .|. toSField n
toOpcode (WR_Assign_RF_Plus_DI_Plus_ALUCIN n) = 0b0100_10_000 .|. toSField n
toOpcode (XWR_Assign_RF_Plus_DI_Plus_ALUCIN n) = 0b0101_10_000 .|. toSField n
toOpcode (RF_Assign_RF_Plus_DI_Plus_ALUCIN n) = 0b0111_10_000 .|. toSField n
toOpcode (WR_Assign_RF_Plus_XWR_Plus_ALUCIN n) = 0b1100_10_000 .|. toSField n
toOpcode (XWR_Assign_RF_Plus_XWR_Plus_ALUCIN n) = 0b1101_10_000 .|. toSField n
toOpcode (RF_Assign_XWR_Plus_ALUCIN n) = 0b1110_10_000 .|. toSField n
toOpcode XWR_Assign_WR_Plus_DI_Plus_ALUCIN = 0b0011_11_010
toOpcode DO_Assign_WR_Plus_DI_Plus_ALUCIN = 0b0111_11_010
toOpcode WR_Assign_XWR_Plus_DI_Plus_ALUCIN = 0b1100_11_010
toOpcode XWR_Assign_XWR_Plus_DI_Plus_ALUCIN = 0b1101_11_010
toOpcode DO_Assign_XWR_Plus_ALUCIN = 0b1110_11_010
-- Group 3: move instructions.
toOpcode (RF_Assign_DI n) = 0b1111_10_000 .|. toSField n
toOpcode (DO_Assign_RF n) = 0b0000_10_000 .|. toSField n
toOpcode (XWR_Assign_RF n) = 0b0001_10_000 .|. toSField n
toOpcode WR_Assign_DI = 0b0110_10_000 -- 0b0110_10_ХХХ or 0b0110_11_010
toOpcode XWR_Assign_DI = 0b0001_11_010
toOpcode DO_Assign_DI = 0b1111_11_010 -- or 0b0000_11_010
-- Group 4: double-precision shift/arithmetic instructions.
toOpcode WRXWR_Assign_RSL_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN = 0b1000_11_010
toOpcode WRXWR_Assign_RSL_WRXWR_Plus_DI_Plus_ALUCIN = 0b1001_11_010
toOpcode (WRXWR_Assign_RSL_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN n) =
  0b1000_10_000 .|. toSField n
toOpcode (WRXWR_Assign_RSL_WRXWR_Plus_RF_Plus_ALUCIN n) =
  0b1001_10_000 .|. toSField n
toOpcode WRXWR_Assign_ASR_WRXWR_Plus_ALUCIN =
  0b1010_10_000 -- 0b1010_10_ХХХ or 0b1010_11_010 or 0b0100_11_101
toOpcode WRXWR_Assign_ASR_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN = 0b0010_11_010
toOpcode WRXWR_Assign_ASR_WRXWR_Plus_DI_Plus_ALUCIN = 0b1011_11_010
toOpcode (WRXWR_Assign_ASR_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN n) =
  0b0010_10_000 .|. toSField n
toOpcode (WRXWR_Assign_ASR_WRXWR_Plus_RF_Plus_ALUCIN n) =
  0b1011_10_000 .|. toSField n
-- Group 5: single-precision shift instructions.
toOpcode WR_Assign_ASR_WR_Plus_ALUCIN = 0b0000_11_101
toOpcode WR_Assign_RSR_WR_Plus_ALUCIN = 0b0001_11_101 -- or 0b1001_11_101
toOpcode WR_Assign_ASL_WR_Plus_ALUCIN = 0b0010_11_101
toOpcode WR_Assign_RSL_WR_Plus_ALUCIN = 0b0011_11_101 -- or 0b1011_11_101
toOpcode WR_Assign_LSR_WR_Plus_ALUCIN = 0b1000_11_101
toOpcode WR_Assign_LSL_WR_Plus_ALUCIN = 0b1010_11_101
-- Group 6: double-precision shift instructions.
toOpcode WRXWR_Assign_RSR_WRXWR_Plus_ALUCIN = 0b0101_11_101 -- or 0b1101_11_101
toOpcode WRXWR_Assign_ASL_WRXWR_Plus_ALUCIN = 0b0110_11_101
toOpcode WRXWR_Assign_RSL_WRXWR_Plus_ALUCIN = 0b0111_11_101 -- or 0b1111_11_101
toOpcode WRXWR_Assign_LSR_WRXWR_Plus_ALUCIN = 0b1100_11_101
toOpcode WRXWR_Assign_LSL_WRXWR_Plus_ALUCIN = 0b1110_11_101
-- Special instructions.
toOpcode No_Operation = 0b0100_11_010

toOpField :: Operation -> Word16
toOpField op = fromIntegral $ fromEnum op `shiftL` 5

toSField :: RFNumber -> Word16
toSField = fromIntegral

toAlucinAttrs :: Maybe AlucinValue -> Word16
toAlucinAttrs (Just True) = hasAlucinAttr .|. alucinValueAttr
toAlucinAttrs (Just False) = hasAlucinAttr
toAlucinAttrs Nothing = 0

toBreakpointAttr :: Breakpoint -> Word16
toBreakpointAttr True = breakpointAttr
toBreakpointAttr False = 0

formatControlStatement :: Map Label Address -> ControlStatement -> Text
formatControlStatement labels (ControlStatement_If cond loc1 (Just loc2)) =
  [i|если #{formatCondition cond} то #{formatLocation labels loc1} |]
    <> [i|иначе #{formatLocation labels loc2}|]
formatControlStatement labels (ControlStatement_If cond loc Nothing) =
  [i|если #{formatCondition cond} то #{formatLocation labels loc}|]
formatControlStatement labels (ControlStatement_Goto loc) =
  [i|иди_на #{formatLocation labels loc}|]
formatControlStatement _ (ControlStatement_Input value) = [i|ввод #{value}|]

formatCondition :: Condition -> Text
formatCondition Condition_ALUCOUT = "П"
formatCondition Condition_ALUCOUT0 = "П0"
formatCondition Condition_ALUCOUT1 = "П1"
formatCondition Condition_ALUCOUT2 = "П2"
formatCondition Condition_Not_WRRT = "!СДП1"
formatCondition Condition_Not_WRLFT = "!СДЛ1"
formatCondition Condition_Not_XWRRT = "!СДП2"
formatCondition Condition_Not_XWRLFT = "!СДЛ2"
formatCondition Condition_XWR0 = "РРР0"
formatCondition Condition_XWR3 = "РРР3"
formatCondition Condition_AMSB = "A15"
formatCondition Condition_BMSB = "B15"

formatLocation :: Map Label Address -> (Location, SourceOffset) -> Text
formatLocation labels (Location_Label label, _) =
  case Map.lookup (T.toCaseFold label) labels of
    Just address -> T.show address
    Nothing -> error "invalid mapping of labels to addresses"
formatLocation _ (Location_Address address, _) = T.show address

-- *** Constants *** --

hasAlucinAttr, alucinValueAttr, breakpointAttr :: Word16
hasAlucinAttr = 0x2000
alucinValueAttr = 0x4000
breakpointAttr = 0x8000
