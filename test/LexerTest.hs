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

module LexerTest where

import Asm584.Lexer
import Asm584.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec_lexer :: Spec
spec_lexer = do
  describe "tokens" $ do
    it "parses a token" $
      parse wrP "" "WR" `shouldParse` WR
    it "parses a Russian token" $
      parse diP "" "ШИНВх" `shouldParse` DI
    it "parses a Russian token case-insensitively" $
      parse doP "" "ШинВых" `shouldParse` DO
    it "skips trailing whitespaces after a token" $
      parse ((,) <$> diP <*> doP <* eof) "" "DI \r\n\t DO \r\n"
        `shouldParse` (DI, DO)
    it "parses a register with the given number" $
      parse (rfP 2) "" "RF2" `shouldParse` RF 2
    it "fails to parse a register with unexpected number" $
      parse (rfP 2) "" `shouldFailOn` "RF3"

  describe "identifiers" $ do
    it "parses an identifier" $
      parse identifierP "" "Name_10" `shouldParse` "Name_10"
    it "parses a Russian identifier" $
      parse identifierP "" "Имя_10" `shouldParse` "Имя_10"
    it "skips trailing whitespaces after an identifier" $
      parse (identifierP <* eof) "" "__myName__ \r\n" `shouldParse` "__myName__"
    it "fails to parse an invalid identifier" $
      parse identifierP "" `shouldFailOn` "10_Name"

  describe "comments" $ do
    it "parses a comment that ends with LF" $
      parse commentP "" "; My Comment\n" `shouldParse` " My Comment"
    it "parses a comment that ends with CRLF" $
      parse commentP "" "# My Comment\r\n" `shouldParse` " My Comment"
    it "skips trailing whitespaces after a comment" $
      parse (commentP <* eof) "" ";Comment\r\n  " `shouldParse` "Comment"
