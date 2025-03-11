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

import Asm584.Parser
import Asm584.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec_parser :: Spec
spec_parser = do
  describe "labels" $ do
    it "parses a label" $
      parse labelP "" "label_2: RF0 := DI" `shouldParse` "label_2"
    it "parses a label with trailing spaces" $
      parse labelP "" "метка_2  : РОН0 := ШИНвх" `shouldParse` "метка_2"
    it "does not confuse a label with the destination operand" $
      parse labelP "" `shouldFailOn` "RF0:=DI"

  describe "instructions" $ do
    describe "group 1: arithmetic/logical instructions" $ do
      testInstruction
        "RF := RF op WR"
        "RF2 := RF2 and WR"
        (RF_Assign_RF_Op_WR A_And_B 2)
      testInstruction
        "WR := RF op WR"
        "РР := !РОН5 или РР"
        (WR_Assign_RF_Op_WR Not_A_Or_B 5)
      testInstruction
        "DO := DI op WR"
        "DO := WR - DI - 1 + C"
        (DO_Assign_DI_Op_WR B_Minus_A_Minus_One_Plus_ALUCIN)
      testInstruction
        "WR := DI op WR"
        "WR=DI-WR-1+C"
        (WR_Assign_DI_Op_WR A_Minus_B_Minus_One_Plus_ALUCIN)
      testInstruction
        "WR := DI op XWR"
        "РР=ШИНвхили!РРР"
        (WR_Assign_DI_Op_XWR A_Or_Not_B)
      testInstruction
        "XWR := DI op WR"
        "xwr := wr + alucin"
        (XWR_Assign_DI_Op_WR B_Plus_ALUCIN)
      testInstruction
        "XWR := DI op XWR"
        "ррр := !ррр + п"
        (XWR_Assign_DI_Op_XWR Not_B_Plus_ALUCIN)
      testInstruction
        "DO := DI op XWR"
        "DO := !(DI xor XWR)"
        (DO_Assign_DI_Op_XWR A_Xnor_B)

  describe "operations" $ do
    describe "arithmetic" $ do
      testOperation "!ALUCIN" DI WR "!ALUCIN" Not_ALUCIN
      testOperation
        "B - A - 1 + ALUCIN"
        DI
        WR
        "WR - DI - 1 + C"
        B_Minus_A_Minus_One_Plus_ALUCIN
      testOperation
        "A - B - 1 + ALUCIN"
        DI
        WR
        "DI - WR - 1 + C"
        A_Minus_B_Minus_One_Plus_ALUCIN
      testOperation
        "A + B + ALUCIN"
        (RF 6)
        WR
        "рон6 + рр + п"
        A_Plus_B_Plus_ALUCIN
      testOperation "B + ALUCIN" DI XWR "XWR+C" B_Plus_ALUCIN
      testOperation "!B + ALUCIN" DI XWR "!XWR+C" Not_B_Plus_ALUCIN
      testOperation "A + ALUCIN" (RF 0) WR "RF0  +  C  " A_Plus_ALUCIN
      testOperation "!A + ALUCIN" (RF 0) WR "!  RF0  +  C  " Not_A_Plus_ALUCIN

    describe "logical" $ do
      testOperation "A and B" DI WR "DI and WR" A_And_B
      testOperation "A xor B" DI XWR "DI xor XWR" A_Xor_B
      testOperation "!(A xor B)" DI XWR "!(DI xor XWR)" A_Xnor_B
      testOperation "!A and B" DI WR "!DIandWR" Not_A_And_B
      testOperation "A and !B" DI WR "DIand!WR" A_And_Not_B
      testOperation "A or !B" (RF 1) WR "РОН1или!РР" A_Or_Not_B
      testOperation "!A or B" (RF 1) WR "!РОН1илиРР" Not_A_Or_B
      testOperation "A or B" (RF 7) WR "RF7 или WR" A_Or_B

  describe "control statements" $ do
    it "parses 'if' statement without 'else' branch" $
      parse controlStatementP "" "if ALUCOUT then label1"
        `shouldParse` ControlStatement_If
          Condition_ALUCOUT
          (Location_Label "label1")
          Nothing
    it "parses 'if' statement with 'else' branch" $
      parse controlStatementP "" "if ALUCOUT then label1 else label2"
        `shouldParse` ControlStatement_If
          Condition_ALUCOUT
          (Location_Label "label1")
          (Just $ Location_Label "label2")
    it "parses 'if' statement with numeric addresses" $
      parse controlStatementP "" "если !СДП2 то 10 иначе 20"
        `shouldParse` ControlStatement_If
          Condition_Not_XWRRT
          (Location_Address 10)
          (Just $ Location_Address 20)
    it "fails to parse 'if' statement with an invalid condition" $
      parse controlStatementP "" `shouldFailOn` "if BadCondition then label1"
    it "fails to parse 'if' statement with an invalid address" $
      parse controlStatementP "" `shouldFailOn` "if ALUCOUT then 2000"
    it "parses 'goto' statement" $
      parse controlStatementP "" "goto label"
        `shouldParse` ControlStatement_Goto (Location_Label "label")
    it "parses 'goto' statement with a numeric address" $
      parse controlStatementP "" "goto 10"
        `shouldParse` ControlStatement_Goto (Location_Address 10)
    it "parses 'input' statement with a 16-digit binary number" $
      parse controlStatementP "" "input 0001001000110100"
        `shouldParse` ControlStatement_Input 0x1234
    it "parses 'input' statement with a 4x4-digit binary number" $
      parse controlStatementP "" "input 0001 0010 0011 0100"
        `shouldParse` ControlStatement_Input 0x1234
    it "parses 'input' statement with a positive decimal number" $
      parse controlStatementP "" "ввод 1234"
        `shouldParse` ControlStatement_Input 1234
    it "parses 'input' statement with a negative decimal number" $
      parse controlStatementP "" "ввод -1234"
        `shouldParse` ControlStatement_Input 64302 -- two's complement
    it "fails to parse 'input' statement with an invalid decimal number" $
      parse controlStatementP "" `shouldFailOn` "input 100500"
    it "parses 'input' statement with a hexadecimal number" $
      parse controlStatementP "" "input 0X789"
        `shouldParse` ControlStatement_Input 0x789
    it "fails to parse 'input' statement with an invalid hexadecimal number" $
      parse controlStatementP "" `shouldFailOn` "input 0xdeadbeef"
  where
    testInstruction name input output =
      it ("parses an instruction '" ++ name ++ "'") $
        parse instructionP "" input `shouldParse` output

    testOperation name a b input output =
      it ("parses an operation '" ++ name ++ "'") $
        parse (operationP a b) "" input `shouldParse` output
