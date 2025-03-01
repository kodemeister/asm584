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
{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import Asm584.Lexer
import Asm584.Parser
import Asm584.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec_parser :: Spec
spec_parser = do
  describe "instructions" $ do
    describe "group 1: arithmetic/logical instructions" $ do
      testInstruction
        "RF := RF op WR"
        "RF2 := RF2 and WR"
        (RF_Assign_RF_Op_WR $ A_And_B (RF 2) WR)
      testInstruction
        "WR := RF op WR"
        "РР := !РОН5 или РР"
        (WR_Assign_RF_Op_WR $ Not_A_Or_B (RF 5) WR)
      testInstruction
        "DO := DI op WR"
        "DO := WR - DI - 1 + C (C=1)"
        (DO_Assign_DI_Op_WR $ B_Minus_A_Minus_One_Plus_ALUCIN DI WR True)
      testInstruction
        "WR := DI op WR"
        "WR=DI-WR-1+C(C=0)"
        (WR_Assign_DI_Op_WR $ A_Minus_B_Minus_One_Plus_ALUCIN DI WR False)
      testInstruction
        "WR := DI op XWR"
        "РР=ШИНвх+РРР+П(П=0)"
        (WR_Assign_DI_Op_XWR $ A_Plus_B_Plus_ALUCIN DI XWR False)
      testInstruction
        "XWR := DI op WR"
        "xwr := di + alucin ( alucin = 1 )"
        (XWR_Assign_DI_Op_WR $ A_Plus_ALUCIN DI True)
      testInstruction
        "XWR := DI op XWR"
        "ррр := !ррр + п ( п = 1 )"
        (XWR_Assign_DI_Op_XWR $ Not_B_Plus_ALUCIN XWR True)
      testInstruction
        "DO := DI op XWR"
        "DO := !(DI xor XWR)"
        (DO_Assign_DI_Op_XWR $ A_Xnor_B DI XWR)

  describe "operations" $ do
    describe "arithmetic" $ do
      testOperation
        "!ALUCIN"
        empty
        empty
        "!ALUCIN (ALUCIN=0)"
        (Not_ALUCIN False)
      testOperation
        "B - A - 1 + ALUCIN"
        diP
        wrP
        "WR - DI - 1 + C (C=1)"
        (B_Minus_A_Minus_One_Plus_ALUCIN DI WR True)
      testOperation
        "A - B - 1 + ALUCIN"
        diP
        wrP
        "DI - WR - 1 + C (C=1)"
        (A_Minus_B_Minus_One_Plus_ALUCIN DI WR True)
      testOperation
        "A + B + ALUCIN"
        rfP
        wrP
        "рон6 + рр + п (п=0)"
        (A_Plus_B_Plus_ALUCIN (RF 6) WR False)
      testOperation
        "B + ALUCIN"
        empty
        xwrP
        "XWR+C(ALUCIN=0)"
        (B_Plus_ALUCIN XWR False)
      testOperation
        "!B + ALUCIN"
        empty
        xwrP
        "!XWR+C(ALUCIN=0)"
        (Not_B_Plus_ALUCIN XWR False)
      testOperation
        "A + ALUCIN"
        (rfWithNumberP 0)
        empty
        "RF0  +  C  ( C = 1 )  "
        (A_Plus_ALUCIN (RF 0) True)
      testOperation
        "!A + ALUCIN"
        (rfWithNumberP 0)
        empty
        "!  RF0  +  C  ( C = 1 )  "
        (Not_A_Plus_ALUCIN (RF 0) True)

    describe "logical" $ do
      testOperation "A and B" diP wrP "DI and WR" (A_And_B DI WR)
      testOperation "A xor B" diP xwrP "DI xor XWR" (A_Xor_B DI XWR)
      testOperation "!(A xor B)" diP xwrP "!(DI xor XWR)" (A_Xnor_B DI XWR)
      testOperation "!A and B" diP wrP "!DIandWR" (Not_A_And_B DI WR)
      testOperation "A and !B" diP wrP "DIand!WR" (A_And_Not_B DI WR)
      testOperation "A or !B" rfP wrP "РОН1или!РР" (A_Or_Not_B (RF 1) WR)
      testOperation "!A or B" rfP wrP "!РОН1илиРР" (Not_A_Or_B (RF 1) WR)
      testOperation "A or B" rfP wrP "RF7 или WR" (A_Or_B (RF 7) WR)
  where
    testInstruction name input output =
      it ("parses an instruction '" ++ name ++ "'") $
        parse instructionP "" input `shouldParse` output

    testOperation name a b input output =
      it ("parses an operation '" ++ name ++ "'") $
        parse (operationP a b) "" input `shouldParse` output
