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

import Asm584.InstructionSet
import Asm584.Lexer
import Asm584.Types
import Control.Applicative.Permutations
import Control.Monad
import Data.Either.Extra (mapLeft)
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import Text.Megaparsec.Error.Builder

-- *** Parsers *** --

parseProgram :: FilePath -> Text -> Either String Program
parseProgram file input = mapLeft errorBundlePretty (parse programP file input)

programP :: Parser Program
programP = do
  statements <- spaces *> count' 1 maxInstructionCount statementP >>= checkEnd
  labels <- getLabels statements
  forM_ (getLocations statements) $ \location ->
    checkLocation labels location
  pure Program {..}
  where
    checkEnd statements = do
      end <- atEnd
      when (length statements == maxInstructionCount && not end) $
        customFailure TooManyInstructions
      statements <$ eof

    getLabels = foldM collectLabel Map.empty . zip [0 ..] . map label

    collectLabel acc (address, Just (label, offset)) = do
      let label' = T.toCaseFold label
      when (label' `Map.member` acc) $
        customFailureAt offset (LabelIsAlreadyDefined label)
      pure $ Map.insert label' address acc
    collectLabel acc _ = pure acc

    getLocations = foldr (collectLocations . controlStatement) []

    collectLocations (Just (ControlStatement_If _ loc1 (Just loc2))) acc =
      loc1 : loc2 : acc
    collectLocations (Just (ControlStatement_If _ loc Nothing)) acc = loc : acc
    collectLocations (Just (ControlStatement_Goto loc)) acc = loc : acc
    collectLocations _ acc = acc

    checkLocation labels (Location_Label label, offset) = do
      let label' = T.toCaseFold label
      when (label' `Map.notMember` labels) $
        customFailureAt offset (LabelIsNotFound label)
    checkLocation _ _ = pure ()

statementP :: Parser Statement
statementP = do
  offset <- getOffset
  label <- optional . try . replaceError offset "label" $ labelP
  breakpoint <- breakpointP
  (instruction, alucin) <- instructionP
  alucinValue <- case alucin of
    NoAlucin -> pure Nothing
    NeedsAlucin -> Just <$> alucinValueP
  (controlStatement, comment) <-
    runPermutation $ (,) <$> toPerm controlStatementP <*> toPerm commentP
  pure Statement {..}
  where
    replaceError offset name = region (const . err offset . elabel $ name)
    toPerm p = toPermutationWithDefault Nothing (Just <$> p)

-- | Parses a label (an identifier followed by colon character).
-- Properly disambiguates labels from destination operands followed by
-- Pascal-style assignment, e.g. "RF0" is not interpreted as label in the input
-- string "RF0:=DI".
labelP :: Parser (Label, SourceOffset)
labelP = do
  offset <- getOffset
  label <- identifierP <* notFollowedBy assignP <* colonP <?> "label"
  pure (label, offset)

breakpointP :: Parser Breakpoint
breakpointP = option BreakpointUnset (BreakpointSet <$ breakP) <?> "breakpoint"

-- | Parses an instruction. The parser is automatically built from the list of
-- all possible instructions.
--
-- NOTE: We reverse the list of instructions before building so that the parser
-- prefers the last matching instruction in case of ambiguity. For instance, the
-- input string "DO := XWR + ALUCIN" matches both instruction "DO := DI op XWR"
-- with operation "B + ALUCIN" and the literal instruction "DO := XWR + ALUCIN".
-- The parser will choose the latter one since it comes later on the list.
instructionP :: Parser (Instruction, Alucin)
instructionP = buildParser (reverse instructions) <?> "microinstruction"

operationP :: Tok -> Tok -> Parser Operation
operationP a b = buildParser $ operations a b

alucinValueP :: Parser AlucinValue
alucinValueP =
  parens (alucinP *> equalP *> (zeroP <|> oneP)) <&> (== One)
    <?> "value of ALUCIN"

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
    <?> "control statement"

conditionP :: Parser Condition
conditionP =
  choice
    [ Condition_ALUCOUT <$ alucoutP,
      Condition_ALUCOUT0 <$ alucout0P,
      Condition_ALUCOUT1 <$ alucout1P,
      Condition_ALUCOUT2 <$ alucout2P,
      notP
        *> choice
          [ Condition_Not_WRRT <$ wrrtP,
            Condition_Not_WRLFT <$ wrlftP,
            Condition_Not_XWRRT <$ xwrrtP,
            Condition_Not_XWRLFT <$ xwrlftP
          ],
      Condition_XWR0 <$ xwr0P,
      Condition_XWR3 <$ xwr3P,
      Condition_AMSB <$ amsbP,
      Condition_BMSB <$ bmsbP
    ]
    <?> "condition"

locationP :: Parser (Location, SourceOffset)
locationP = do
  offset <- getOffset
  location <-
    (Location_Label <$> identifierP <?> "label")
      <|> (Location_Address <$> addressP <?> "address")
  pure (location, offset)

addressP :: Parser Address
addressP = do
  offset <- getOffset
  address <- decimal
  if address < toInteger maxInstructionCount
    then pure $ fromInteger address
    else customFailureAt offset (AddressIsOutOfRange address)

-- | Parses a 16-bit input value in one of the following formats:
-- 1. A 16-digit binary number.
-- 2. A binary number divided into 4 groups of 4 digits separated by space.
-- 3. A hexadecimal number.
-- 4. A signed or unsigned decimal number.
inputValueP :: Parser Word16
inputValueP = do
  offset <- getOffset
  value <- choice formats <?> "16-bit binary, decimal or hexadecimal number"
  if value >= -32768 && value <= 65535
    then pure $ fromInteger value
    else customFailureAt offset (InputValueIsOutOfRange value)
  where
    formats =
      [ try $ binaryN 16,
        try $ binaryMxN 4 4 spaceChar,
        hexadecimal,
        signed decimal
      ]

-- | Builds a parser from an association list of token sequences. The resulting
-- parser will match the input stream against all token sequences and return the
-- associated value "a" if it finds any matching sequence.
--
-- The function basically performs the following steps:
-- 1. Splits each token sequence into the first token and the rest of sequence.
-- 2. Groups all sequences by their first token (i.e. by the common prefix).
-- 3. For each group it builds a parser that accepts the first token and then
--    recursively parses the rest of sequences.
-- 4. Combines all group parsers via 'choice' combinator.
buildParser :: [(TokenSequence, a)] -> Parser a
buildParser = choice . map sequenceP . groupSequences . map splitSequence
  where
    splitSequence (ts, a) = (listToMaybe ts, [(drop 1 ts, a)])

    -- NOTE: We sort groups by their first token in the descending order so that
    -- (Just tok) keys come first and Nothing key comes last. This implements
    -- the "maximal munch" rule when the parser tries to consume as much tokens
    -- as possible before finishing parsing and returning the resulting value at
    -- Nothing key. We also preserve the original order of token sequences in
    -- each group.
    groupSequences = Map.toDescList . Map.fromListWith (++) . reverse

    sequenceP (Just tok, tss) = tokenP tok *> buildParser tss
    sequenceP (Nothing, ([], a) : _) = pure a
    sequenceP (Nothing, _) = error "invalid association list of token sequences"

-- *** Utilities *** --

parens :: Parser a -> Parser a
parens = between openParenP closeParenP

customFailureAt :: SourceOffset -> ParserError -> Parser a
customFailureAt offset = parseError . errFancy offset . fancy . ErrorCustom
