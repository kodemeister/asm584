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
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}

module CodeGenTest where

import Asm584.CodeGen
import Asm584.Types
import Test.Hspec

spec_codegen :: Spec
spec_codegen = do
  describe "instructions" $ do
    it "encodes an instruction" $
      encodeInstruction (WR_Assign_RF_Op_WR A_Xor_B 6) Nothing False
        `shouldBe` 0b000_0000_1001_01_110
    it "encodes an instruction with a value of ALUCIN" $
      encodeInstruction (WR_Assign_RF_Plus_XWR_Plus_ALUCIN 3) (Just True) False
        `shouldBe` 0b011_0000_1100_10_011
    it "encodes an instruction with a breakpoint" $
      encodeInstruction XWR_Assign_DI Nothing True
        `shouldBe` 0b100_0000_0001_11_010
