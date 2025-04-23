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

module Asm584.InstructionSet where

import Asm584.Types
import Data.List

instructions :: [(TokenSequence, Instruction, Alucin)]
instructions =
  map checkAlucin . concat $
    [ -- Group 1: arithmetic/logical instructions.
      [ ([RF n, Assign] ++ ts, RF_Assign_RF_Op_WR op n)
        | n <- rfNumbers,
          (ts, op, _) <- operations (RF n) WR
      ],
      [ ([WR, Assign] ++ ts, WR_Assign_RF_Op_WR op n)
        | n <- rfNumbers,
          (ts, op, _) <- operations (RF n) WR
      ],
      [ ([DO, Assign] ++ ts, DO_Assign_DI_Op_WR op)
        | (ts, op, _) <- operations DI WR
      ],
      [ ([WR, Assign] ++ ts, WR_Assign_DI_Op_WR op)
        | (ts, op, _) <- operations DI WR
      ],
      [ ([WR, Assign] ++ ts, WR_Assign_DI_Op_XWR op)
        | (ts, op, _) <- operations DI XWR
      ],
      [ ([XWR, Assign] ++ ts, XWR_Assign_DI_Op_WR op)
        | (ts, op, _) <- operations DI WR
      ],
      [ ([XWR, Assign] ++ ts, XWR_Assign_DI_Op_XWR op)
        | (ts, op, _) <- operations DI XWR
      ],
      [ ([DO, Assign] ++ ts, DO_Assign_DI_Op_XWR op)
        | (ts, op, _) <- operations DI XWR
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
      [ ( [WR, Assign, RF n, Plus, XWR, Plus, ALUCIN],
          WR_Assign_RF_Plus_XWR_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( [XWR, Assign, RF n, Plus, XWR, Plus, ALUCIN],
          XWR_Assign_RF_Plus_XWR_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ([RF n, Assign, XWR, Plus, ALUCIN], RF_Assign_XWR_Plus_ALUCIN n)
        | n <- rfNumbers
      ],
      [ ( [XWR, Assign, WR, Plus, DI, Plus, ALUCIN],
          XWR_Assign_WR_Plus_DI_Plus_ALUCIN
        )
      ],
      [ ( [DO, Assign, WR, Plus, DI, Plus, ALUCIN],
          DO_Assign_WR_Plus_DI_Plus_ALUCIN
        )
      ],
      [ ( [WR, Assign, XWR, Plus, DI, Plus, ALUCIN],
          WR_Assign_XWR_Plus_DI_Plus_ALUCIN
        )
      ],
      [ ( [XWR, Assign, XWR, Plus, DI, Plus, ALUCIN],
          XWR_Assign_XWR_Plus_DI_Plus_ALUCIN
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
      [ ( wrxwrShift RSL [WR, Minus, DI, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_RSL_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN
        )
      ],
      [ ( wrxwrShift RSL [WR, Plus, DI, Plus, ALUCIN],
          WRXWR_Assign_RSL_WRXWR_Plus_DI_Plus_ALUCIN
        )
      ],
      [ ( wrxwrShift RSL [WR, Minus, RF n, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_RSL_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( wrxwrShift RSL [WR, Plus, RF n, Plus, ALUCIN],
          WRXWR_Assign_RSL_WRXWR_Plus_RF_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( wrxwrShift ASR [WR, Plus, ALUCIN],
          WRXWR_Assign_ASR_WRXWR_Plus_ALUCIN
        )
      ],
      [ ( wrxwrShift ASR [WR, Minus, DI, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_ASR_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN
        )
      ],
      [ ( wrxwrShift ASR [WR, Plus, DI, Plus, ALUCIN],
          WRXWR_Assign_ASR_WRXWR_Plus_DI_Plus_ALUCIN
        )
      ],
      [ ( wrxwrShift ASR [WR, Minus, RF n, Minus, One, Plus, ALUCIN],
          WRXWR_Assign_ASR_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      [ ( wrxwrShift ASR [WR, Plus, RF n, Plus, ALUCIN],
          WRXWR_Assign_ASR_WRXWR_Plus_RF_Plus_ALUCIN n
        )
        | n <- rfNumbers
      ],
      -- Group 5: single-precision shift instructions.
      [(wrShift ASR wrPlusAlucin, WR_Assign_ASR_WR_Plus_ALUCIN)],
      [(wrShift RSR wrPlusAlucin, WR_Assign_RSR_WR_Plus_ALUCIN)],
      [(wrShift ASL wrPlusAlucin, WR_Assign_ASL_WR_Plus_ALUCIN)],
      [(wrShift RSL wrPlusAlucin, WR_Assign_RSL_WR_Plus_ALUCIN)],
      [(wrShift LSR wrPlusAlucin, WR_Assign_LSR_WR_Plus_ALUCIN)],
      [(wrShift LSL wrPlusAlucin, WR_Assign_LSL_WR_Plus_ALUCIN)],
      -- Group 6: double-precision shift instructions.
      [(wrxwrShift RSR wrPlusAlucin, WRXWR_Assign_RSR_WRXWR_Plus_ALUCIN)],
      [(wrxwrShift ASL wrPlusAlucin, WRXWR_Assign_ASL_WRXWR_Plus_ALUCIN)],
      [(wrxwrShift RSL wrPlusAlucin, WRXWR_Assign_RSL_WRXWR_Plus_ALUCIN)],
      [(wrxwrShift LSR wrPlusAlucin, WRXWR_Assign_LSR_WRXWR_Plus_ALUCIN)],
      [(wrxwrShift LSL wrPlusAlucin, WRXWR_Assign_LSL_WRXWR_Plus_ALUCIN)],
      -- Special instructions.
      [([NOP], No_Operation)]
    ]
  where
    checkAlucin (ts, instr) =
      (ts, instr, if ALUCIN `elem` ts then NeedsAlucin else NoAlucin)
    rfNumbers = [0 .. 7]
    wrShift shift expr = [WR, Assign, shift, OpenParen] ++ expr ++ [CloseParen]
    wrxwrShift shift expr =
      [OpenParen, WR, Comma, XWR, CloseParen, Assign, shift, OpenParen]
        ++ expr
        ++ [Comma, XWR, CloseParen]
    wrPlusAlucin = [WR, Plus, ALUCIN]

operations :: Tok -> Tok -> [(TokenSequence, Operation, Alucin)]
operations a b =
  concat
    [ -- Arithmetic operations.
      operation expr1 Not_ALUCIN,
      operation (expr4 b a) B_Minus_A_Minus_One_Plus_ALUCIN,
      operation (expr4 a b) A_Minus_B_Minus_One_Plus_ALUCIN,
      operation (expr3 a b) A_Plus_B_Plus_ALUCIN,
      operation (expr2 [b]) B_Plus_ALUCIN,
      operation (expr2 [Not, b]) Not_B_Plus_ALUCIN,
      operation (expr2 [a]) A_Plus_ALUCIN,
      operation (expr2 [Not, a]) Not_A_Plus_ALUCIN,
      -- Logical operations.
      operation (op2 [a] And [b]) A_And_B,
      operation (op2 [a] Xor [b]) A_Xor_B,
      operation (inverted $ op2 [a] Xor [b]) A_Xnor_B,
      operation (op2 [Not, a] And [b]) Not_A_And_B,
      operation (op2 [a] And [Not, b]) A_And_Not_B,
      operation (op2 [a] Or [Not, b]) A_Or_Not_B,
      operation (op2 [Not, a] Or [b]) Not_A_Or_B,
      operation (op2 [a] Or [b]) A_Or_B
    ]
  where
    operation expr op = [(ts, op, alucin) | (ts, alucin) <- expr]
    inverted expr =
      [([Not, OpenParen] ++ ts ++ [CloseParen], alucin) | (ts, alucin) <- expr]

-- | Returns token sequences for the arithmetic expression "!ALUCIN".
expr1 :: [(TokenSequence, Alucin)]
expr1 = [([Not, ALUCIN], NeedsAlucin)]

-- | Returns token sequences for the arithmetic expression "A + ALUCIN".
expr2 :: TokenSequence -> [(TokenSequence, Alucin)]
expr2 a =
  [(ts, NeedsAlucin) | ts <- [a ++ [Plus, ALUCIN], [ALUCIN, Plus] ++ a]]

-- | Returns token sequences for the arithmetic expression "A + B + ALUCIN".
expr3 :: Tok -> Tok -> [(TokenSequence, Alucin)]
expr3 a b =
  [(intersperse Plus p, NeedsAlucin) | p <- permutations [a, b, ALUCIN]]

-- | Returns token sequences for the arithmetic expression "A - B - 1 + ALUCIN".
expr4 :: Tok -> Tok -> [(TokenSequence, Alucin)]
expr4 a b =
  [ (dropWhile (== Plus) (concat p), NeedsAlucin)
    | p <- permutations [[Plus, a], [Minus, b], [Minus, One], [Plus, ALUCIN]]
  ]

-- | Returns token sequences for the logical expression "A op B".
op2 :: TokenSequence -> Tok -> TokenSequence -> [(TokenSequence, Alucin)]
op2 a op b = [(ts, NoAlucin) | ts <- [a ++ op : b, b ++ op : a]]
