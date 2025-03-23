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

module Asm584.Lexer where

import Asm584.Types
import Data.Char
import Data.Foldable
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- *** Tokens *** --

wrP, xwrP :: Parser Tok
wrP = WR <$ oneOfKeywords ["WR", "РР"]
xwrP = XWR <$ oneOfKeywords ["XWR", "РРР"]

rfP :: RFNumber -> Parser Tok
rfP n
  | n >= 0 && n <= 7 = RF n <$ oneOfKeywords [[i|RF#{n}|], [i|РОН#{n}|]]
  | otherwise = error "invalid register number"

diP, doP :: Parser Tok
diP = DI <$ oneOfKeywords ["DI", "ШИНвх", "ШВх"]
doP = DO <$ oneOfKeywords ["DO", "ШИНвых", "ШВых"]

alucinP :: Parser Tok
alucinP = ALUCIN <$ oneOfKeywords ["ALUCIN", "C", "П"]

alucoutP, alucout0P, alucout1P, alucout2P :: Parser Tok
alucoutP = ALUCOUT <$ oneOfKeywords ["ALUCOUT", "CO3", "C", "ПАЛУ3", "П"]
alucout0P = ALUCOUT0 <$ oneOfKeywords ["ALUCOUT0", "CO0", "C0", "ПАЛУ0", "П0"]
alucout1P = ALUCOUT1 <$ oneOfKeywords ["ALUCOUT1", "CO1", "C1", "ПАЛУ1", "П1"]
alucout2P = ALUCOUT2 <$ oneOfKeywords ["ALUCOUT2", "CO2", "C2", "ПАЛУ2", "П2"]

wrrtP, wrlftP, xwrrtP, xwrlftP :: Parser Tok
wrrtP = WRRT <$ oneOfKeywords ["WRRT", "SR1", "СДП1"]
wrlftP = WRLFT <$ oneOfKeywords ["WRLFT", "SL1", "СДЛ1"]
xwrrtP = XWRRT <$ oneOfKeywords ["XWRRT", "SR2", "СДП2"]
xwrlftP = XWRLFT <$ oneOfKeywords ["XWRLFT", "SL2", "СДЛ2"]

xwr0P, xwr3P, amsbP, bmsbP :: Parser Tok
xwr0P = XWR0 <$ oneOfKeywords ["XWR0", "РРР0"]
xwr3P = XWR3 <$ oneOfKeywords ["XWR3", "РРР3"]
amsbP = AMSB <$ oneOfKeywords ["AMSB", "A15"]
bmsbP = BMSB <$ oneOfKeywords ["BMSB", "B15"]

plusP, minusP, zeroP, oneP :: Parser Tok
plusP = Plus <$ symbol "+"
minusP = Minus <$ symbol "-"
zeroP = Zero <$ symbol "0"
oneP = One <$ symbol "1"

andP, orP, xorP, notP :: Parser Tok
andP = And <$ oneOfKeywords ["and", "и"]
orP = Or <$ oneOfKeywords ["or", "или"]
xorP = Xor <$ oneOfKeywords ["xor", "искл_или"]
notP = Not <$ symbol "!"

lsrP, lslP, asrP, aslP, rsrP, rslP :: Parser Tok
lsrP = LSR <$ oneOfKeywords ["LSR", "СЛП"]
lslP = LSL <$ oneOfKeywords ["LSL", "СЛЛ"]
asrP = ASR <$ oneOfKeywords ["ASR", "САП"]
aslP = ASL <$ oneOfKeywords ["ASL", "САЛ"]
rsrP = RSR <$ oneOfKeywords ["RSR", "СЦП"]
rslP = RSL <$ oneOfKeywords ["RSL", "СЦЛ"]

nopP, breakP :: Parser Tok
nopP = NOP <$ oneOfKeywords ["NOP", "НОП", "<ПУСТО>"]
breakP = Break <$ oneOfKeywords ["break", "останов"]

ifP, thenP, elseP, gotoP, inputP :: Parser Tok
ifP = If <$ oneOfKeywords ["if", "если"]
thenP = Then <$ oneOfKeywords ["then", "то"]
elseP = Else <$ oneOfKeywords ["else", "иначе"]
gotoP = Goto <$ oneOfKeywords ["goto", "иди_на"]
inputP = Input <$ oneOfKeywords ["input", "ввод"]

assignP, equalP, openParenP, closeParenP, commaP, colonP :: Parser Tok
assignP = Assign <$ oneOfSymbols [":=", "="]
equalP = Equal <$ symbol "="
openParenP = OpenParen <$ symbol "("
closeParenP = CloseParen <$ symbol ")"
commaP = Comma <$ symbol ","
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
tokenP NOP = nopP
tokenP Break = breakP
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
    (satisfy isCommentChar <?> "comment")
      *> takeWhileP (Just "any character except newline") isNotNewline
  where
    isCommentChar c = c == ';' || c == '#'
    isNotNewline c = c /= '\r' && c /= '\n'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

oneOfSymbols :: [Text] -> Parser Text
oneOfSymbols = choice . map symbol

keyword :: Text -> Parser Text
keyword str =
  try . lexeme $ string' str <* notFollowedBy (alphaNumChar <|> char '_')

oneOfKeywords :: [Text] -> Parser Text
oneOfKeywords = choice . map keyword

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
