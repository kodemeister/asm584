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
{-# LANGUAGE RecordWildCards #-}

module Asm584.Parser where

import Asm584.Lexer
import Asm584.Types
import Control.Monad.Extra (whenMaybe)
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char

-- *** Parsers *** --

statementP :: Parser Statement
statementP = do
  label <- optional $ try labelP
  breakpoint <- isJust <$> optional breakpointP
  instruction <- instructionP
  alucinValue <- whenMaybe (hasAlucin instruction) alucinValueP
  (controlStatement, comment) <- optionalPair controlStatementP commentP
  pure Statement {..}

-- | Parses a label (an identifier followed by colon character).
-- Properly disambiguates labels from destination operands followed by
-- Pascal-style assignment, e.g. "RF0" is not interpreted as label in the input
-- string "RF0:=DI".
labelP :: Parser Label
labelP = (identifierP <?> "label") <* notFollowedBy assignP <* colonP

breakpointP :: Parser Tok
breakpointP = breakP <?> "breakpoint"

-- | Parses an instruction. The parser is automatically built from the list of
-- all possible instructions.
--
-- NOTE: We reverse the list of instructions before building so that the parser
-- prefers the last matching instruction in case of ambiguity. For instance, the
-- instruction "XWR := DI + WR + ALUCIN" matches both instruction #6 with
-- operation "A + B + ALUCIN" and instruction #16. The parser will choose the
-- latter one.
instructionP :: Parser Instruction
instructionP = buildParser (reverse instructions) <?> "microinstruction"

operationP :: Tok -> Tok -> Parser Operation
operationP a b = buildParser $ operations a b

alucinValueP :: Parser Bool
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

locationP :: Parser Location
locationP =
  (Location_Label <$> identifierP <?> "label")
    <|> (Location_Address <$> addressP <?> "address")
  where
    addressP = fromInteger <$> valueInRange (0, maxInstructions - 1) decimal

-- | Parses a 16-bit input value in one of the following formats:
-- 1. A 16-digit binary number.
-- 2. A binary number divided into 4 groups of 4 digits separated by space.
-- 3. A hexadecimal number.
-- 4. A signed or unsigned decimal number.
inputValueP :: Parser Word16
inputValueP =
  choice
    [ try $ fromInteger <$> binaryN 16,
      try $ fromInteger <$> binaryMxN 4 4 spaceChar,
      fromInteger <$> valueInRange (0, 65535) hexadecimal,
      fromInteger <$> valueInRange (-32768, 65535) (signed decimal)
    ]
    <?> "16-bit binary, decimal or hexadecimal number"

-- *** Parser builder *** --

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
    sequenceP (Nothing, _) = empty

hasAlucin :: Instruction -> Bool
hasAlucin instr = instr `Set.member` instructionsWithAlucin

instructionsWithAlucin :: Set Instruction
instructionsWithAlucin =
  Set.fromList [instr | (ts, instr) <- instructions, ALUCIN `elem` ts]

instructions :: [(TokenSequence, Instruction)]
instructions =
  concat
    [ -- Group 1: arithmetic/logical instructions.
      [ ([RF n, Assign] ++ ts, RF_Assign_RF_Op_WR op n)
        | n <- rfNumbers,
          (ts, op) <- operations (RF n) WR
      ],
      [ ([WR, Assign] ++ ts, WR_Assign_RF_Op_WR op n)
        | n <- rfNumbers,
          (ts, op) <- operations (RF n) WR
      ],
      [ ([DO, Assign] ++ ts, DO_Assign_DI_Op_WR op)
        | (ts, op) <- operations DI WR
      ],
      [ ([WR, Assign] ++ ts, WR_Assign_DI_Op_WR op)
        | (ts, op) <- operations DI WR
      ],
      [ ([WR, Assign] ++ ts, WR_Assign_DI_Op_XWR op)
        | (ts, op) <- operations DI XWR
      ],
      [ ([XWR, Assign] ++ ts, XWR_Assign_DI_Op_WR op)
        | (ts, op) <- operations DI WR
      ],
      [ ([XWR, Assign] ++ ts, XWR_Assign_DI_Op_XWR op)
        | (ts, op) <- operations DI XWR
      ],
      [ ([DO, Assign] ++ ts, DO_Assign_DI_Op_XWR op)
        | (ts, op) <- operations DI XWR
      ],
      -- Group 2: addition instructions.
      [ ( [XWR, Assign, RF n, Plus, WR, Plus, ALUCIN],
          XWR_Assign_RF_Plus_WR_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( [WR, Assign, RF n, Plus, DI, Plus, ALUCIN],
          WR_Assign_RF_Plus_DI_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( [XWR, Assign, RF n, Plus, DI, Plus, ALUCIN],
          XWR_Assign_RF_Plus_DI_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( [RF n, Assign, RF n, Plus, DI, Plus, ALUCIN],
          RF_Assign_RF_Plus_DI_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( [XWR, Assign, RF n, Plus, XWR, Plus, ALUCIN],
          XWR_Assign_RF_Plus_XWR_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( [WR, Assign, RF n, Plus, XWR, Plus, ALUCIN],
          WR_Assign_RF_Plus_XWR_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ([RF n, Assign, XWR, Plus, ALUCIN], RF_Assign_XWR_Plus_ALUCIN n)
        | n <- rfNumbers
      ],
      [ ( [XWR, Assign, DI, Plus, WR, Plus, ALUCIN],
          XWR_Assign_DI_Plus_WR_Plus_ALUCIN
        )
      ],
      [ ( [DO, Assign, DI, Plus, WR, Plus, ALUCIN],
          DO_Assign_DI_Plus_WR_Plus_ALUCIN
        )
      ],
      [ ( [WR, Assign, DI, Plus, XWR, Plus, ALUCIN],
          WR_Assign_DI_Plus_XWR_Plus_ALUCIN
        )
      ],
      [ ( [XWR, Assign, DI, Plus, XWR, Plus, ALUCIN],
          XWR_Assign_DI_Plus_XWR_Plus_ALUCIN
        )
      ],
      [([DO, Assign, XWR, Plus, ALUCIN], DO_Assign_XWR_Plus_ALUCIN)],
      -- Group 3: move instructions.
      [([RF n, Assign, DI], RF_Assign_DI n) | n <- rfNumbers],
      [([DO, Assign, RF n], DO_Assign_RF n) | n <- rfNumbers],
      [([XWR, Assign, RF n], XWR_Assign_RF n) | n <- rfNumbers],
      [([WR, Assign, DI], WR_Assign_DI)],
      [([XWR, Assign, DI], XWR_Assign_DI)],
      [([DO, Assign, DI], DO_Assign_DI)],
      -- Group 4: double-precision shift/arithmetic instructions.
      [ ( wrxwrAssign ++ rsl [WR, Minus, DI, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_RSL_WR_Minus_DI_Minus_One_Plus_ALUCIN
        )
      ],
      [ ( wrxwrAssign ++ rsl [WR, Plus, DI, Plus, ALUCIN],
          WRXWR_Assign_RSL_WR_Plus_DI_Plus_ALUCIN
        )
      ],
      [ ( wrxwrAssign ++ rsl [WR, Minus, RF n, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_RSL_WR_Minus_RF_Minus_One_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( wrxwrAssign ++ rsl [WR, Plus, RF n, Plus, ALUCIN],
          WRXWR_Assign_RSL_WR_Plus_RF_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( wrxwrAssign ++ asr [WR, Plus, ALUCIN],
          WRXWR_Assign_ASR_WR_Plus_ALUCIN
        )
      ],
      [ ( wrxwrAssign ++ asr [WR, Minus, DI, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_ASR_WR_Minus_DI_Minus_One_Plus_ALUCIN
        )
      ],
      [ ( wrxwrAssign ++ asr [WR, Plus, DI, Plus, ALUCIN],
          WRXWR_Assign_ASR_WR_Plus_DI_Plus_ALUCIN
        )
      ],
      [ ( wrxwrAssign ++ asr [WR, Minus, RF n, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_ASR_WR_Minus_RF_Minus_One_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( wrxwrAssign ++ asr [WR, Plus, RF n, Plus, ALUCIN],
          WRXWR_Assign_ASR_WR_Plus_RF_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      -- Group 5: single-precision shift instructions.
      [([WR, Assign] ++ asr [WR, Plus, ALUCIN], WR_Assign_ASR_WR_Plus_ALUCIN)],
      [([WR, Assign] ++ rsr [WR, Plus, ALUCIN], WR_Assign_RSR_WR_Plus_ALUCIN)],
      [([WR, Assign] ++ asl [WR, Plus, ALUCIN], WR_Assign_ASL_WR_Plus_ALUCIN)],
      [([WR, Assign] ++ rsl [WR, Plus, ALUCIN], WR_Assign_RSL_WR_Plus_ALUCIN)],
      [([WR, Assign] ++ lsr [WR, Plus, ALUCIN], WR_Assign_LSR_WR_Plus_ALUCIN)],
      [([WR, Assign] ++ lsl [WR, Plus, ALUCIN], WR_Assign_LSL_WR_Plus_ALUCIN)],
      -- Group 6: double-precision shift instructions.
      [(wrxwrAssign ++ asr wrxwrAlucin, WRXWR_Assign_ASR_WRXWR_Plus_ALUCIN)],
      [(wrxwrAssign ++ rsr wrxwrAlucin, WRXWR_Assign_RSR_WRXWR_Plus_ALUCIN)],
      [(wrxwrAssign ++ asl wrxwrAlucin, WRXWR_Assign_ASL_WRXWR_Plus_ALUCIN)],
      [(wrxwrAssign ++ rsl wrxwrAlucin, WRXWR_Assign_RSL_WRXWR_Plus_ALUCIN)],
      [(wrxwrAssign ++ lsr wrxwrAlucin, WRXWR_Assign_LSR_WRXWR_Plus_ALUCIN)],
      [(wrxwrAssign ++ lsl wrxwrAlucin, WRXWR_Assign_LSL_WRXWR_Plus_ALUCIN)],
      -- Special instructions.
      [([NOP], No_Operation)]
    ]
  where
    rfNumbers = [0 .. 7]
    wrxwrAssign = [OpenParen, WR, Comma, XWR, CloseParen, Assign]
    wrxwrAlucin = [WR, Plus, ALUCIN, Comma, XWR]
    lsr ts = [LSR, OpenParen] ++ ts ++ [CloseParen]
    lsl ts = [LSL, OpenParen] ++ ts ++ [CloseParen]
    asr ts = [ASR, OpenParen] ++ ts ++ [CloseParen]
    asl ts = [ASL, OpenParen] ++ ts ++ [CloseParen]
    rsr ts = [RSR, OpenParen] ++ ts ++ [CloseParen]
    rsl ts = [RSL, OpenParen] ++ ts ++ [CloseParen]

operations :: Tok -> Tok -> [(TokenSequence, Operation)]
operations a b =
  [ -- Arithmetic operations.
    ([Not, ALUCIN], Not_ALUCIN),
    ([b, Minus, a, Minus, One, Plus, ALUCIN], B_Minus_A_Minus_One_Plus_ALUCIN),
    ([a, Minus, b, Minus, One, Plus, ALUCIN], A_Minus_B_Minus_One_Plus_ALUCIN),
    ([a, Plus, b, Plus, ALUCIN], A_Plus_B_Plus_ALUCIN),
    ([b, Plus, ALUCIN], B_Plus_ALUCIN),
    ([Not, b, Plus, ALUCIN], Not_B_Plus_ALUCIN),
    ([a, Plus, ALUCIN], A_Plus_ALUCIN),
    ([Not, a, Plus, ALUCIN], Not_A_Plus_ALUCIN),
    -- Logical operations.
    ([a, And, b], A_And_B),
    ([a, Xor, b], A_Xor_B),
    ([Not, OpenParen, a, Xor, b, CloseParen], A_Xnor_B),
    ([Not, a, And, b], Not_A_And_B),
    ([a, And, Not, b], A_And_Not_B),
    ([a, Or, Not, b], A_Or_Not_B),
    ([Not, a, Or, b], Not_A_Or_B),
    ([a, Or, b], A_Or_B)
  ]

-- *** Utilities *** --

maxInstructions :: (Num a) => a
maxInstructions = 1024

parens :: Parser a -> Parser a
parens = between openParenP closeParenP

-- | Parses a pair of optional values that can appear in any order.
optionalPair :: Parser a -> Parser b -> Parser (Maybe a, Maybe b)
optionalPair a b =
  choice
    [ (,) . Just <$> a <*> optional b,
      flip (,) . Just <$> b <*> optional a,
      pure (Nothing, Nothing)
    ]

valueInRange :: (Ord a, Show a) => (a, a) -> Parser a -> Parser a
valueInRange (min', max') p = do
  value <- p
  if value >= min' && value <= max'
    then pure value
    else fail $ "value is out of range " ++ range
  where
    range = "[" ++ show min' ++ ", " ++ show max' ++ "]"
