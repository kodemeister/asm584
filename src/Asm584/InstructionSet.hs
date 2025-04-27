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
  concat
    [ -- Group 1: arithmetic/logical instructions.
      forallRF $ \n -> instruction1 (RF n) (RF n) WR RF_Assign_RF_Op_WR,
      forallRF $ \n -> instruction1 WR (RF n) WR WR_Assign_RF_Op_WR,
      instruction1 DO DI WR DO_Assign_DI_Op_WR,
      instruction1 WR DI WR WR_Assign_DI_Op_WR,
      instruction1 WR DI XWR WR_Assign_DI_Op_XWR,
      instruction1 XWR DI WR XWR_Assign_DI_Op_WR,
      instruction1 XWR DI XWR XWR_Assign_DI_Op_XWR,
      instruction1 DO DI XWR DO_Assign_DI_Op_XWR,
      -- Group 2: addition instructions.
      forallRF $ \n ->
        instruction2 XWR (expr3 (RF n) WR) XWR_Assign_RF_Plus_WR_Plus_ALUCIN,
      forallRF $ \n ->
        instruction2 WR (expr3 (RF n) DI) WR_Assign_RF_Plus_DI_Plus_ALUCIN,
      forallRF $ \n ->
        instruction2 XWR (expr3 (RF n) DI) XWR_Assign_RF_Plus_DI_Plus_ALUCIN,
      forallRF $ \n ->
        instruction2 (RF n) (expr3 (RF n) DI) RF_Assign_RF_Plus_DI_Plus_ALUCIN,
      forallRF $ \n ->
        instruction2 WR (expr3 (RF n) XWR) WR_Assign_RF_Plus_XWR_Plus_ALUCIN,
      forallRF $ \n ->
        instruction2 XWR (expr3 (RF n) XWR) XWR_Assign_RF_Plus_XWR_Plus_ALUCIN,
      forallRF $ \n ->
        instruction2 (RF n) (expr2 [XWR]) RF_Assign_XWR_Plus_ALUCIN,
      instruction2 XWR (expr3 WR DI) XWR_Assign_WR_Plus_DI_Plus_ALUCIN,
      instruction2 DO (expr3 WR DI) DO_Assign_WR_Plus_DI_Plus_ALUCIN,
      instruction2 WR (expr3 XWR DI) WR_Assign_XWR_Plus_DI_Plus_ALUCIN,
      instruction2 XWR (expr3 XWR DI) XWR_Assign_XWR_Plus_DI_Plus_ALUCIN,
      instruction2 DO (expr2 [XWR]) DO_Assign_XWR_Plus_ALUCIN,
      -- Group 3: move instructions.
      forallRF $ \n -> instruction3 (RF n) DI RF_Assign_DI,
      forallRF $ \n -> instruction3 DO (RF n) DO_Assign_RF,
      forallRF $ \n -> instruction3 XWR (RF n) XWR_Assign_RF,
      instruction3 WR DI WR_Assign_DI,
      instruction3 XWR DI XWR_Assign_DI,
      instruction3 DO DI DO_Assign_DI,
      -- Group 4: double-precision shift/arithmetic instructions.
      instruction4 RSL (expr4 WR DI) $
        WRXWR_Assign_RSL_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN,
      instruction4 RSL (expr3 WR DI) $
        WRXWR_Assign_RSL_WRXWR_Plus_DI_Plus_ALUCIN,
      forallRF $ \n ->
        instruction4 RSL (expr4 WR (RF n)) $
          WRXWR_Assign_RSL_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN,
      forallRF $ \n ->
        instruction4 RSL (expr3 WR (RF n)) $
          WRXWR_Assign_RSL_WRXWR_Plus_RF_Plus_ALUCIN,
      instruction4 ASR (expr2 [WR]) WRXWR_Assign_ASR_WRXWR_Plus_ALUCIN,
      instruction4 ASR (expr4 WR DI) $
        WRXWR_Assign_ASR_WRXWR_Minus_DI_Minus_One_Plus_ALUCIN,
      instruction4 ASR (expr3 WR DI) $
        WRXWR_Assign_ASR_WRXWR_Plus_DI_Plus_ALUCIN,
      forallRF $ \n ->
        instruction4 ASR (expr4 WR (RF n)) $
          WRXWR_Assign_ASR_WRXWR_Minus_RF_Minus_One_Plus_ALUCIN,
      forallRF $ \n ->
        instruction4 ASR (expr3 WR (RF n)) $
          WRXWR_Assign_ASR_WRXWR_Plus_RF_Plus_ALUCIN,
      -- Group 5: single-precision shift instructions.
      instruction5 ASR WR_Assign_ASR_WR_Plus_ALUCIN,
      instruction5 RSR WR_Assign_RSR_WR_Plus_ALUCIN,
      instruction5 ASL WR_Assign_ASL_WR_Plus_ALUCIN,
      instruction5 RSL WR_Assign_RSL_WR_Plus_ALUCIN,
      instruction5 LSR WR_Assign_LSR_WR_Plus_ALUCIN,
      instruction5 LSL WR_Assign_LSL_WR_Plus_ALUCIN,
      -- Group 6: double-precision shift instructions.
      instruction6 RSR WRXWR_Assign_RSR_WRXWR_Plus_ALUCIN,
      instruction6 ASL WRXWR_Assign_ASL_WRXWR_Plus_ALUCIN,
      instruction6 RSL WRXWR_Assign_RSL_WRXWR_Plus_ALUCIN,
      instruction6 LSR WRXWR_Assign_LSR_WRXWR_Plus_ALUCIN,
      instruction6 LSL WRXWR_Assign_LSL_WRXWR_Plus_ALUCIN,
      -- Special instructions.
      [([NOP], No_Operation, NoAlucin)]
    ]
  where
    instruction1 dest a b instr =
      [ (dest : Assign : ts, instr op, alucin)
        | (ts, op, alucin) <- operations a b
      ]
    instruction2 dest expr instr =
      [(dest : Assign : ts, instr, alucin) | (ts, alucin) <- expr]
    instruction3 dest src instr = [([dest, Assign, src], instr, NoAlucin)]
    instruction4 shift expr instr =
      [(dest ++ Assign : shifted ts, instr, alucin) | (ts, alucin) <- expr]
      where
        dest = [OpenParen, WR, Comma, XWR, CloseParen]
        shifted ts = [shift, OpenParen] ++ ts ++ [Comma, XWR, CloseParen]
    instruction5 shift instr =
      [(WR : Assign : shifted ts, instr, alucin) | (ts, alucin) <- expr2 [WR]]
      where
        shifted ts = [shift, OpenParen] ++ ts ++ [CloseParen]
    instruction6 shift = instruction4 shift (expr2 [WR])
    forallRF f =
      [(ts, instr n, alucin) | n <- [0 .. 7], (ts, instr, alucin) <- f n]

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
expr1 =
  [ ([Not, ALUCIN], NeedsAlucin),
    ([Not, Zero], OmitsAlucin False),
    ([Not, One], OmitsAlucin True),
    ([Zero], OmitsAlucin True)
  ]

-- | Returns token sequences for the arithmetic expression "A + ALUCIN".
expr2 :: TokenSequence -> [(TokenSequence, Alucin)]
expr2 a =
  concat
    [ [(ts, NeedsAlucin) | ts <- perms2 ALUCIN],
      [(ts, OmitsAlucin False) | ts <- perms2 Zero],
      [(ts, OmitsAlucin True) | ts <- perms2 One],
      [(a, OmitsAlucin False)]
    ]
  where
    perms2 c = [a ++ [Plus, c], [c, Plus] ++ a]

-- | Returns token sequences for the arithmetic expression "A + B + ALUCIN".
expr3 :: Tok -> Tok -> [(TokenSequence, Alucin)]
expr3 a b =
  concat
    [ [(ts, NeedsAlucin) | ts <- perms3 ALUCIN],
      [(ts, OmitsAlucin False) | ts <- perms3 Zero],
      [(ts, OmitsAlucin True) | ts <- perms3 One],
      [(ts, OmitsAlucin False) | ts <- perms2]
    ]
  where
    perms3 c = map (intersperse Plus) (permutations [a, b, c])
    perms2 = [[a, Plus, b], [b, Plus, a]]

-- | Returns token sequences for the arithmetic expression "A - B - 1 + ALUCIN".
expr4 :: Tok -> Tok -> [(TokenSequence, Alucin)]
expr4 a b =
  concat
    [ [(ts, NeedsAlucin) | ts <- perms4 ALUCIN],
      [(ts, OmitsAlucin False) | ts <- perms4 Zero],
      [(ts, OmitsAlucin True) | ts <- perms4 One],
      [(ts, OmitsAlucin False) | ts <- perms3],
      [(ts, OmitsAlucin True) | ts <- perms2]
    ]
  where
    perms4 c = perms [[Plus, a], [Minus, b], [Minus, One], [Plus, c]]
    perms3 = perms [[Plus, a], [Minus, b], [Minus, One]]
    perms2 = [[a, Minus, b], [Minus, b, Plus, a]]
    perms = map (dropWhile (== Plus) . concat) . permutations

-- | Returns token sequences for the logical expression "A op B".
op2 :: TokenSequence -> Tok -> TokenSequence -> [(TokenSequence, Alucin)]
op2 a op b = [(ts, NoAlucin) | ts <- [a ++ op : b, b ++ op : a]]
