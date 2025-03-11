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

module Asm584.Lexer where

import Asm584.Types
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- *** Tokens *** --

wrP :: Parser Tok
wrP = WR <$ oneOfSymbols' ["WR", "РР"]

xwrP :: Parser Tok
xwrP = XWR <$ oneOfSymbols' ["XWR", "РРР"]

rfP :: RFNumber -> Parser Tok
rfP n
  | n >= 0 && n <= 7 = RF n <$ oneOfSymbols' (["RF", "РОН"] <&> (<> T.show n))
  | otherwise = error "invalid register number"

diP :: Parser Tok
diP = DI <$ oneOfSymbols' ["DI", "ШИНвх", "ШВх"]

doP :: Parser Tok
doP = DO <$ oneOfSymbols' ["DO", "ШИНвых", "ШВых"]

alucinP :: Parser Tok
alucinP = ALUCIN <$ oneOfSymbols' ["ALUCIN", "C", "П"]

alucoutP :: Parser Tok
alucoutP = ALUCOUT <$ oneOfSymbols' ["ALUCOUT", "CO3", "C", "ПАЛУ3", "П"]

alucout0P :: Parser Tok
alucout0P = ALUCOUT0 <$ oneOfSymbols' ["ALUCOUT0", "CO0", "C0", "ПАЛУ0", "П0"]

alucout1P :: Parser Tok
alucout1P = ALUCOUT1 <$ oneOfSymbols' ["ALUCOUT1", "CO1", "C1", "ПАЛУ1", "П1"]

alucout2P :: Parser Tok
alucout2P = ALUCOUT2 <$ oneOfSymbols' ["ALUCOUT2", "CO2", "C2", "ПАЛУ2", "П2"]

wrrtP :: Parser Tok
wrrtP = WRRT <$ oneOfSymbols' ["WRRT", "SR1", "СДП1"]

wrlftP :: Parser Tok
wrlftP = WRLFT <$ oneOfSymbols' ["WRLFT", "SL1", "СДЛ1"]

xwrrtP :: Parser Tok
xwrrtP = XWRRT <$ oneOfSymbols' ["XWRRT", "SR2", "СДП2"]

xwrlftP :: Parser Tok
xwrlftP = XWRLFT <$ oneOfSymbols' ["XWRLFT", "SL2", "СДЛ2"]

xwr0P :: Parser Tok
xwr0P = XWR0 <$ oneOfSymbols' ["XWR0", "РРР0"]

xwr3P :: Parser Tok
xwr3P = XWR3 <$ oneOfSymbols' ["XWR3", "РРР3"]

amsbP :: Parser Tok
amsbP = AMSB <$ oneOfSymbols' ["AMSB", "A15"]

bmsbP :: Parser Tok
bmsbP = BMSB <$ oneOfSymbols' ["BMSB", "B15"]

plusP :: Parser Tok
plusP = Plus <$ symbol "+"

minusP :: Parser Tok
minusP = Minus <$ symbol "-"

zeroP :: Parser Tok
zeroP = Zero <$ symbol "0"

oneP :: Parser Tok
oneP = One <$ symbol "1"

andP :: Parser Tok
andP = And <$ oneOfSymbols' ["and", "и"]

orP :: Parser Tok
orP = Or <$ oneOfSymbols' ["or", "или"]

xorP :: Parser Tok
xorP = Xor <$ oneOfSymbols' ["xor", "искл_или"]

notP :: Parser Tok
notP = Not <$ symbol "!"

lsrP :: Parser Tok
lsrP = LSR <$ oneOfSymbols' ["LSR", "СЛП"]

lslP :: Parser Tok
lslP = LSL <$ oneOfSymbols' ["LSL", "СЛЛ"]

asrP :: Parser Tok
asrP = ASR <$ oneOfSymbols' ["ASR", "САП"]

aslP :: Parser Tok
aslP = ASL <$ oneOfSymbols' ["ASL", "САЛ"]

rsrP :: Parser Tok
rsrP = RSR <$ oneOfSymbols' ["RSR", "СЦП"]

rslP :: Parser Tok
rslP = RSL <$ oneOfSymbols' ["RSL", "СЦЛ"]

ifP :: Parser Tok
ifP = If <$ oneOfSymbols' ["if", "если"]

thenP :: Parser Tok
thenP = Then <$ oneOfSymbols' ["then", "то"]

elseP :: Parser Tok
elseP = Else <$ oneOfSymbols' ["else", "иначе"]

gotoP :: Parser Tok
gotoP = Goto <$ oneOfSymbols' ["goto", "иди_на"]

inputP :: Parser Tok
inputP = Input <$ oneOfSymbols' ["input", "ввод"]

assignP :: Parser Tok
assignP = Assign <$ oneOfSymbols [":=", "="]

equalP :: Parser Tok
equalP = Equal <$ symbol "="

openParenP :: Parser Tok
openParenP = OpenParen <$ symbol "("

closeParenP :: Parser Tok
closeParenP = CloseParen <$ symbol ")"

commaP :: Parser Tok
commaP = Comma <$ symbol ","

colonP :: Parser Tok
colonP = Colon <$ symbol ":"

tokenP :: Tok -> Parser Tok
tokenP WR = wrP
tokenP XWR = xwrP
tokenP (RF n) = rfP n
tokenP DI = diP
tokenP DO = doP
tokenP ALUCIN = alucinP
tokenP ALUCOUT = alucoutP
tokenP ALUCOUT0 = alucout0P
tokenP ALUCOUT1 = alucout1P
tokenP ALUCOUT2 = alucout2P
tokenP WRRT = wrrtP
tokenP WRLFT = wrlftP
tokenP XWRRT = xwrrtP
tokenP XWRLFT = xwrlftP
tokenP XWR0 = xwr0P
tokenP XWR3 = xwr3P
tokenP AMSB = amsbP
tokenP BMSB = bmsbP
tokenP Plus = plusP
tokenP Minus = minusP
tokenP Zero = zeroP
tokenP One = oneP
tokenP And = andP
tokenP Or = orP
tokenP Xor = xorP
tokenP Not = notP
tokenP LSR = lsrP
tokenP LSL = lslP
tokenP ASR = asrP
tokenP ASL = aslP
tokenP RSR = rsrP
tokenP RSL = rslP
tokenP If = ifP
tokenP Then = thenP
tokenP Else = elseP
tokenP Goto = gotoP
tokenP Input = inputP
tokenP Assign = assignP
tokenP Equal = equalP
tokenP OpenParen = openParenP
tokenP CloseParen = closeParenP
tokenP Comma = commaP
tokenP Colon = colonP

-- *** Utilities *** --

identifierP :: Parser Text
identifierP =
  lexeme $
    T.cons
      <$> (satisfy isHeadChar <?> "letter or underscore character")
      <*> takeWhileP (Just "letter, digit or underscore character") isTailChar
  where
    isHeadChar c = isAlpha c || c == '_'
    isTailChar c = isAlphaNum c || c == '_'

commentP :: Parser Text
commentP =
  lexeme $
    (satisfy isCommentChar <?> "comment character")
      *> takeWhileP (Just "any character except newline") isNotNewline
  where
    isCommentChar c = c == ';' || c == '#'
    isNotNewline c = c /= '\r' && c /= '\n'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

-- | Case-insensitive version of 'symbol'.
symbol' :: Text -> Parser Text
symbol' = L.symbol' spaces

oneOfSymbols :: [Text] -> Parser Text
oneOfSymbols = choice . map symbol

-- | Case-insensitive version of 'oneOfSymbols'.
oneOfSymbols' :: [Text] -> Parser Text
oneOfSymbols' = choice . map symbol'

signed :: (Num a) => Parser a -> Parser a
signed = L.signed spaces

-- | Parses a N-digit binary number.
binaryN :: Int -> Parser Integer
binaryN n = binaryToInteger <$> lexeme (count n binDigitChar)

-- | Parses a binary number divided into M groups of N digits separated by the
-- given delimiter.
binaryMxN :: Int -> Int -> Parser sep -> Parser Integer
binaryMxN m n sep =
  binaryToInteger
    <$> ((++) . concat <$> count (m - 1) (group <* sep) <*> lexeme group)
  where
    group = count n binDigitChar

binaryToInteger :: String -> Integer
binaryToInteger = foldl' (\acc c -> acc * 2 + toInteger (digitToInt c)) 0

decimal :: Parser Integer
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme $ string' "0x" *> L.hexadecimal

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
