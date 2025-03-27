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

module CodeGenTest where

import Asm584.CodeGen
import Asm584.Types
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T
import Test.Hspec

spec_codegen :: Spec
spec_codegen = do
  describe "programs" $ do
    it "encodes a simple program" $
      encodeProgram
        Program
          { statements =
              [ defStatement
                  { instruction = XWR_Assign_DI,
                    controlStatement = Just $ ControlStatement_Input 1234,
                    comment = Just " Hello"
                  },
                defStatement
                  { instruction = DO_Assign_XWR_Plus_ALUCIN,
                    alucinValue = Just True,
                    comment = Just "Test Comment"
                  }
              ],
            labels = Map.empty
          }
        `shouldBe` B.concat
          [ "X584",
            "\x3A\x00", -- 0b000_0000_0001_11_010
            "\x09\xE2\xE2\xEE\xE4 1234",
            "\xDA\x61", -- 0b011_0000_1110_11_010
            "\x0CTest Comment",
            B.concat $ replicate 1022 "\x9A\x00\x00",
            "V2.0",
            "\x0D\xD0\xB2\xD0\xB2\xD0\xBE\xD0\xB4 1234",
            "\x05Hello",
            "\x00",
            "\x0CTest Comment",
            B.concat $ replicate 1022 "\x00\x00"
          ]

  describe "statements" $ do
    it "encodes a statement" $
      encodeStatement
        (Map.singleton "метка" 13)
        ( Statement
            Nothing
            BreakpointSet
            WR_Assign_ASR_WR_Plus_ALUCIN
            (Just False)
            (Just $ ControlStatement_Goto (Location_Label "Метка", 0))
            (Just " My Comment ")
        )
        `shouldBe` ( 0b101_0000_0000_11_101,
                     "\xE8\xE4\xE8_\xED\xE0 13",
                     "\xD0\xB8\xD0\xB4\xD0\xB8_\xD0\xBD\xD0\xB0 13",
                     "My Comment"
                   )
    it "encodes a statement with a long comment" $
      encodeStatement
        Map.empty
        defStatement {comment = Just $ T.replicate 200 "П"}
        `shouldBe` ( 0b000_0000_1111_11_010,
                     B.replicate 128 0xCF,
                     "",
                     B.concat $ replicate 200 "\xD0\x9F"
                   )
    it "encodes a statement with a comment containing Chinese characters" $
      encodeStatement
        Map.empty
        defStatement {comment = Just "Hello, 世界!"}
        `shouldBe` ( 0b000_0000_1111_11_010,
                     "Hello, ??!",
                     "",
                     "Hello, \xE4\xB8\x96\xE7\x95\x8C!"
                   )

  describe "instructions" $ do
    it "encodes an instruction" $
      encodeInstruction (WR_Assign_RF_Op_WR A_Xor_B 6) Nothing BreakpointUnset
        `shouldBe` 0b000_0000_1001_01_110
    it "encodes an instruction with a value of ALUCIN" $
      encodeInstruction WR_Assign_ASL_WR_Plus_ALUCIN (Just True) BreakpointUnset
        `shouldBe` 0b011_0000_0010_11_101
    it "encodes an instruction with a breakpoint" $
      encodeInstruction XWR_Assign_DI Nothing BreakpointSet
        `shouldBe` 0b100_0000_0001_11_010

  describe "control statements" $ do
    it "formats 'if' statement without 'else' branch" $
      formatControlStatement
        Map.empty
        ( ControlStatement_If
            Condition_ALUCOUT2
            (Location_Address 10, 0)
            Nothing
        )
        `shouldBe` "если П2 то 10"
    it "formats 'if' statement with 'else' branch" $
      formatControlStatement
        (Map.fromList [("label1", 10), ("метка2", 20), ("label3", 30)])
        ( ControlStatement_If
            Condition_Not_WRLFT
            (Location_Label "Label1", 0)
            (Just (Location_Label "МЕТКА2", 0))
        )
        `shouldBe` "если !СДЛ1 то 10 иначе 20"
    it "formats 'goto' statement" $
      formatControlStatement
        (Map.singleton "переполнение" 666)
        (ControlStatement_Goto (Location_Label "Переполнение", 0))
        `shouldBe` "иди_на 666"
    it "formats 'input' statement" $
      formatControlStatement
        Map.empty
        (ControlStatement_Input 60000)
        `shouldBe` "ввод 60000"
  where
    defStatement =
      Statement Nothing BreakpointUnset DO_Assign_DI Nothing Nothing Nothing
