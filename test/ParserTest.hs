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

module ParserTest where

import Asm584.Parser
import Asm584.Types
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (label)

spec_parser :: Spec
spec_parser = do
  describe "programs" $ do
    it "parses a simple program" $
      parse
        programP
        ""
        "/* Hello */ One: DO := DI goto TWO DO := DI ; Skipped\nTwo: DO := DI"
        `shouldParse` Program
          { statements =
              [ defStatement
                  { label = Just ("One", 12),
                    controlStatement =
                      Just $ ControlStatement_Goto (Location_Label "TWO", 31)
                  },
                defStatement {comment = Just " Skipped"},
                defStatement {label = Just ("Two", 54)}
              ],
            labels = Map.fromList [("one", 0), ("two", 2)]
          }
    it "fails to parse a program with too many instructions" $
      parse programP "" `shouldFailOn` T.unwords (replicate 1025 "DO := DI")
    it "fails to parse a program with duplicated labels" $
      parse programP "" `shouldFailOn` "метка: ШВых := РОН0\nМеТкА: ШВых := ШВх"
    it "fails to parse a program with an invalid label in 'goto' statement" $
      parse programP "" `shouldFailOn` "DO := DI goto invalid\nlabel: DO := DI"

  describe "statements" $ do
    testStatement
      "with a single instruction"
      "DO := DI"
      defStatement
    testStatement
      "with a label"
      "my_label: DO := DI"
      defStatement {label = Just ("my_label", 0)}
    testStatement
      "with a breakpoint"
      "break DO := DI"
      defStatement {breakpoint = BreakpointSet}
    testStatement
      "with a value of ALUCIN"
      "DO := XWR + C (C=1)"
      defStatement
        { instruction = DO_Assign_XWR_Plus_ALUCIN,
          alucinValue = Just True
        }
    it "fails to parse a statement if a value of ALUCIN is missing" $
      parse statementP "" `shouldFailOn` "DO := XWR + C"
    testStatement
      "with a control statement"
      "DO := DI goto 10"
      defStatement
        { controlStatement =
            Just $ ControlStatement_Goto (Location_Address 10, 14)
        }
    testStatement
      "with a comment"
      "DO := DI ; Comment"
      defStatement {comment = Just " Comment"}
    testStatement
      "with a control statement followed by a comment"
      "DO := DI\ngoto 20\n; My Comment"
      defStatement
        { controlStatement =
            Just $ ControlStatement_Goto (Location_Address 20, 14),
          comment = Just " My Comment"
        }
    testStatement
      "with a comment followed by a control statement"
      "DO := DI\n; My Comment\ngoto 30"
      defStatement
        { controlStatement =
            Just $ ControlStatement_Goto (Location_Address 30, 27),
          comment = Just " My Comment"
        }

  describe "labels" $ do
    it "parses a label" $
      parse labelP "" "label_2: RF0 := DI" `shouldParse` ("label_2", 0)
    it "parses a label with trailing spaces" $
      parse labelP "" "метка_2  : РОН0 := ШИНвх" `shouldParse` ("метка_2", 0)
    it "does not confuse a label with the destination operand" $
      parse labelP "" `shouldFailOn` "RF0:=DI"

  describe "instructions" $ do
    describe "group 1: arithmetic/logical instructions" $ do
      testInstruction
        "RF := RF op WR"
        "RF2 := RF2 and WR"
        (RF_Assign_RF_Op_WR A_And_B 2, NoAlucin)
      testInstruction
        "WR := RF op WR"
        "РР := !РОН5 или РР"
        (WR_Assign_RF_Op_WR Not_A_Or_B 5, NoAlucin)
      testInstruction
        "DO := DI op WR"
        "DO := WR - DI - 1 + C"
        (DO_Assign_DI_Op_WR B_Minus_A_Minus_One_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := DI op WR"
        "WR=DI-WR-1+C"
        (WR_Assign_DI_Op_WR A_Minus_B_Minus_One_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := DI op XWR"
        "РР=ШИНвх или!РРР"
        (WR_Assign_DI_Op_XWR A_Or_Not_B, NoAlucin)
      testInstruction
        "XWR := DI op WR"
        "xwr := wr + alucin"
        (XWR_Assign_DI_Op_WR B_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "XWR := DI op XWR"
        "ррр := !ррр + п"
        (XWR_Assign_DI_Op_XWR Not_B_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "DO := DI op XWR"
        "DO := !(DI xor XWR)"
        (DO_Assign_DI_Op_XWR A_Xnor_B, NoAlucin)

    describe "group 2: addition instructions" $ do
      testInstruction
        "XWR := RF + WR + ALUCIN"
        "XWR := RF1 + WR + ALUCIN"
        (XWR_Assign_RF_Plus_WR_Plus_ALUCIN 1, NeedsAlucin)
      testInstruction
        "WR := RF + DI + ALUCIN"
        "WR := RF2 + DI + C"
        (WR_Assign_RF_Plus_DI_Plus_ALUCIN 2, NeedsAlucin)
      testInstruction
        "XWR := RF + DI + ALUCIN"
        "XWR:=RF3+DI+C"
        (XWR_Assign_RF_Plus_DI_Plus_ALUCIN 3, NeedsAlucin)
      testInstruction
        "RF := RF + DI + ALUCIN"
        "RF4  :=  RF4  +  DI  +  C  "
        (RF_Assign_RF_Plus_DI_Plus_ALUCIN 4, NeedsAlucin)
      testInstruction
        "WR := RF + XWR + ALUCIN"
        "РР := РОН5 + РРР + П"
        (WR_Assign_RF_Plus_XWR_Plus_ALUCIN 5, NeedsAlucin)
      testInstruction
        "XWR := RF + XWR + ALUCIN"
        "РРР:=РОН6+РРР+П"
        (XWR_Assign_RF_Plus_XWR_Plus_ALUCIN 6, NeedsAlucin)
      testInstruction
        "RF := XWR + ALUCIN"
        "РОН7  :=  РРР  +  П  "
        (RF_Assign_XWR_Plus_ALUCIN 7, NeedsAlucin)
      testInstruction
        "XWR := WR + DI + ALUCIN"
        "xwr = wr + di + alucin"
        (XWR_Assign_WR_Plus_DI_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "DO := WR + DI + ALUCIN"
        "do=wr+di+alucin"
        (DO_Assign_WR_Plus_DI_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := XWR + DI + ALUCIN"
        "рр = ррр + шинвх + п"
        (WR_Assign_XWR_Plus_DI_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "XWR := XWR + DI + ALUCIN"
        "ррр=ррр+шинвх+п"
        (XWR_Assign_XWR_Plus_DI_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "DO := XWR + ALUCIN"
        "ШВых := XWR + П"
        (DO_Assign_XWR_Plus_ALUCIN, NeedsAlucin)

    describe "group 3: move instructions" $ do
      testInstruction "RF := DI" "RF0 = DI" (RF_Assign_DI 0, NoAlucin)
      testInstruction "DO := RF" "DO = RF2" (DO_Assign_RF 2, NoAlucin)
      testInstruction "XWR := RF" "XWR = RF4" (XWR_Assign_RF 4, NoAlucin)
      testInstruction "WR := DI" "WR = DI" (WR_Assign_DI, NoAlucin)
      testInstruction "XWR := DI" "XWR = DI" (XWR_Assign_DI, NoAlucin)
      testInstruction "DO := DI" "DO = DI" (DO_Assign_DI, NoAlucin)

    describe "group 4: double-precision shift/arithmetic instructions" $ do
      testInstruction
        "(WR, XWR) := RSL(WR - DI - 1 + ALUCIN, XWR)"
        "(WR, XWR) := RSL(WR - DI - 1 + ALUCIN, XWR)"
        (WRXWR_Assign_RSL_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := RSL(WR + DI + ALUCIN, XWR)"
        "(WR, XWR) := RSL(WR + DI + ALUCIN, XWR)"
        (WRXWR_Assign_RSL_WRXWR_Plus_DI_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := RSL(WR - RF - 1 + ALUCIN, XWR)"
        "(WR, XWR) := RSL(WR - RF3 - 1 + ALUCIN, XWR)"
        (WRXWR_Assign_RSL_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN 3, NeedsAlucin)
      testInstruction
        "(WR, XWR) := RSL(WR + RF + ALUCIN, XWR)"
        "(WR, XWR) := RSL(WR + RF5 + ALUCIN, XWR)"
        (WRXWR_Assign_RSL_WRXWR_Plus_RF_Plus_ALUCIN 5, NeedsAlucin)
      testInstruction
        "(WR, XWR) := ASR(WR + ALUCIN, XWR)"
        "(WR, XWR) := ASR(WR + ALUCIN, XWR)"
        (WRXWR_Assign_ASR_WRXWR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := ASR(WR - DI - 1 + ALUCIN, XWR)"
        "(WR, XWR) := ASR(WR - DI - 1 + ALUCIN, XWR)"
        (WRXWR_Assign_ASR_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := ASR(WR + DI + ALUCIN, XWR)"
        "(WR, XWR) := ASR(WR + DI + ALUCIN, XWR)"
        (WRXWR_Assign_ASR_WRXWR_Plus_DI_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := ASR(WR - RF - 1 + ALUCIN, XWR)"
        "(WR, XWR) := ASR(WR - RF1 - 1 + ALUCIN, XWR)"
        (WRXWR_Assign_ASR_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN 1, NeedsAlucin)
      testInstruction
        "(WR, XWR) := ASR(WR + RF + ALUCIN, XWR)"
        "(WR, XWR) := ASR(WR + RF7 + ALUCIN, XWR)"
        (WRXWR_Assign_ASR_WRXWR_Plus_RF_Plus_ALUCIN 7, NeedsAlucin)

    describe "group 5: single-precision shift instructions" $ do
      testInstruction
        "WR := ASR(WR + ALUCIN)"
        "wr = asr ( wr+c )"
        (WR_Assign_ASR_WR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := RSR(WR + ALUCIN)"
        "wr = rsr ( wr+c )"
        (WR_Assign_RSR_WR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := ASL(WR + ALUCIN)"
        "wr = asl ( wr+c )"
        (WR_Assign_ASL_WR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := RSL(WR + ALUCIN)"
        "wr = rsl ( wr+c )"
        (WR_Assign_RSL_WR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := LSR(WR + ALUCIN)"
        "wr = lsr ( wr+c )"
        (WR_Assign_LSR_WR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "WR := LSL(WR + ALUCIN)"
        "wr = lsl ( wr+c )"
        (WR_Assign_LSL_WR_Plus_ALUCIN, NeedsAlucin)

    describe "group 6: double-precision shift instructions" $ do
      testInstruction
        "(WR, XWR) := RSR(WR + ALUCIN, XWR)"
        "(РР, РРР) := СЦП(РР + П, РРР)"
        (WRXWR_Assign_RSR_WRXWR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := ASL(WR + ALUCIN, XWR)"
        "(РР, РРР) := САЛ(РР + П, РРР)"
        (WRXWR_Assign_ASL_WRXWR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := RSL(WR + ALUCIN, XWR)"
        "(РР, РРР) := СЦЛ(РР + П, РРР)"
        (WRXWR_Assign_RSL_WRXWR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := LSR(WR + ALUCIN, XWR)"
        "(РР, РРР) := СЛП(РР + П, РРР)"
        (WRXWR_Assign_LSR_WRXWR_Plus_ALUCIN, NeedsAlucin)
      testInstruction
        "(WR, XWR) := LSL(WR + ALUCIN, XWR)"
        "(РР, РРР) := СЛЛ(РР + П, РРР)"
        (WRXWR_Assign_LSL_WRXWR_Plus_ALUCIN, NeedsAlucin)

    describe "special instructions" $ do
      testInstruction "NOP" "НОП" (No_Operation, NoAlucin)

    describe "reordered operands" $ do
      testInstructionWithReorderedOperands
        "DO := DI op WR"
        "DO := WR and DI"
        (DO_Assign_DI_Op_WR A_And_B, NoAlucin)
      testInstructionWithReorderedOperands
        "XWR := WR + DI + ALUCIN"
        "XWR := ALUCIN + DI + WR"
        (XWR_Assign_WR_Plus_DI_Plus_ALUCIN, NeedsAlucin)
      testInstructionWithReorderedOperands
        "(WR, XWR) := RSL(WR - RF - 1 + ALUCIN, XWR)"
        "(WR, XWR) := RSL(-1 + ALUCIN + WR - RF2, XWR)"
        (WRXWR_Assign_RSL_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN 2, NeedsAlucin)
      testInstructionWithReorderedOperands
        "WR := RSR(WR + ALUCIN)"
        "WR := RSR(ALUCIN + WR)"
        (WR_Assign_RSR_WR_Plus_ALUCIN, NeedsAlucin)
      testInstructionWithReorderedOperands
        "(WR, XWR) := RSR(WR + ALUCIN, XWR)"
        "(WR, XWR) := RSR(ALUCIN + WR, XWR)"
        (WRXWR_Assign_RSR_WRXWR_Plus_ALUCIN, NeedsAlucin)

  describe "operations" $ do
    describe "arithmetic" $ do
      testOperation
        "!ALUCIN"
        DI
        WR
        "!ALUCIN"
        (Not_ALUCIN, NeedsAlucin)
      testOperation
        "B - A - 1 + ALUCIN"
        DI
        WR
        "WR - DI - 1 + C"
        (B_Minus_A_Minus_One_Plus_ALUCIN, NeedsAlucin)
      testOperation
        "A - B - 1 + ALUCIN"
        DI
        WR
        "DI - WR - 1 + C"
        (A_Minus_B_Minus_One_Plus_ALUCIN, NeedsAlucin)
      testOperation
        "A + B + ALUCIN"
        (RF 6)
        WR
        "рон6 + рр + п"
        (A_Plus_B_Plus_ALUCIN, NeedsAlucin)
      testOperation
        "B + ALUCIN"
        DI
        XWR
        "XWR+C"
        (B_Plus_ALUCIN, NeedsAlucin)
      testOperation
        "!B + ALUCIN"
        DI
        XWR
        "!XWR+C"
        (Not_B_Plus_ALUCIN, NeedsAlucin)
      testOperation
        "A + ALUCIN"
        (RF 0)
        WR
        "RF0  +  C  "
        (A_Plus_ALUCIN, NeedsAlucin)
      testOperation
        "!A + ALUCIN"
        (RF 0)
        WR
        "!  RF0  +  C  "
        (Not_A_Plus_ALUCIN, NeedsAlucin)

    describe "logical" $ do
      testOperation "A and B" DI WR "DI and WR" (A_And_B, NoAlucin)
      testOperation "A xor B" DI XWR "DI xor XWR" (A_Xor_B, NoAlucin)
      testOperation "!(A xor B)" DI XWR "!(DI xor XWR)" (A_Xnor_B, NoAlucin)
      testOperation "!A and B" DI WR "!DI and WR" (Not_A_And_B, NoAlucin)
      testOperation "A and !B" DI WR "DI and!WR" (A_And_Not_B, NoAlucin)
      testOperation "A or !B" (RF 1) WR "РОН1 или!РР" (A_Or_Not_B, NoAlucin)
      testOperation "!A or B" (RF 1) WR "!РОН1 или РР" (Not_A_Or_B, NoAlucin)
      testOperation "A or B" (RF 7) WR "RF7 или WR" (A_Or_B, NoAlucin)

    describe "reordered operands" $ do
      testOperationWithReorderedOperands
        "A + ALUCIN"
        (RF 0)
        WR
        "ALUCIN + RF0"
        (A_Plus_ALUCIN, NeedsAlucin)
      testOperationWithReorderedOperands
        "A or !B"
        (RF 1)
        WR
        "!WR or RF1"
        (A_Or_Not_B, NoAlucin)
      testOperationWithReorderedOperands
        "A + B + ALUCIN"
        DI
        WR
        "WR + DI + ALUCIN"
        (A_Plus_B_Plus_ALUCIN, NeedsAlucin)
      testOperationWithReorderedOperands
        "A - B - 1 + ALUCIN"
        DI
        XWR
        "ALUCIN - XWR + DI - 1"
        (A_Minus_B_Minus_One_Plus_ALUCIN, NeedsAlucin)
      testOperationWithReorderedOperands
        "B - A - 1 + ALUCIN"
        DI
        XWR
        "-DI + XWR + ALUCIN - 1"
        (B_Minus_A_Minus_One_Plus_ALUCIN, NeedsAlucin)

  describe "control statements" $ do
    it "parses 'if' statement without 'else' branch" $
      parse controlStatementP "" "if ALUCOUT then label1"
        `shouldParse` ControlStatement_If
          Condition_ALUCOUT
          (Location_Label "label1", 16)
          Nothing
    it "parses 'if' statement with 'else' branch" $
      parse controlStatementP "" "if ALUCOUT then label1 else label2"
        `shouldParse` ControlStatement_If
          Condition_ALUCOUT
          (Location_Label "label1", 16)
          (Just (Location_Label "label2", 28))
    it "parses 'if' statement with numeric addresses" $
      parse controlStatementP "" "если !СДП2 то 10 иначе 20"
        `shouldParse` ControlStatement_If
          Condition_Not_XWRRT
          (Location_Address 10, 14)
          (Just (Location_Address 20, 23))
    it "fails to parse 'if' statement with an invalid condition" $
      parse controlStatementP "" `shouldFailOn` "if BadCondition then label1"
    it "fails to parse 'if' statement with an invalid address" $
      parse controlStatementP "" `shouldFailOn` "if ALUCOUT then 2000"
    it "parses 'goto' statement" $
      parse controlStatementP "" "goto label"
        `shouldParse` ControlStatement_Goto (Location_Label "label", 5)
    it "parses 'goto' statement with a numeric address" $
      parse controlStatementP "" "goto 10"
        `shouldParse` ControlStatement_Goto (Location_Address 10, 5)
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
    testStatement name input output =
      it [i|parses a statement #{name :: String}|] $
        parse statementP "" input `shouldParse` output

    defStatement =
      Statement Nothing BreakpointUnset DO_Assign_DI Nothing Nothing Nothing

    testInstruction name input output =
      it [i|parses an instruction '#{name :: String}'|] $
        parse instructionP "" input `shouldParse` output

    testInstructionWithReorderedOperands name =
      testInstructionWith name "reordered operands"

    testInstructionWith name with input output =
      it [i|parses an instruction '#{name :: String}' with #{with :: String}|] $
        parse instructionP "" input `shouldParse` output

    testOperation name a b input output =
      it [i|parses an operation '#{name :: String}'|] $
        parse (operationP a b) "" input `shouldParse` output

    testOperationWithReorderedOperands name =
      testOperationWith name "reordered operands"

    testOperationWith name with a b input output =
      it [i|parses an operation '#{name :: String}' with #{with :: String}|] $
        parse (operationP a b) "" input `shouldParse` output
