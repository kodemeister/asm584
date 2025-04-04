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

module Main (main) where

import Asm584
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Encoding
import Data.Encoding.CP1251
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options.Applicative
import System.Environment
import System.Exit
import System.FilePath
import System.IO.CodePage (withCP65001)

main :: IO ()
main = withCP65001 . handle showAppError $ do
  options <- parseCommandLine
  input <- readInputFile options
  output <- assembleProgram options input
  writeOutputFile options output

parseCommandLine :: IO Options
parseCommandLine = do
  result <- parseArguments <$> getArgs
  case result of
    Success options -> pure options
    _ -> handleParseResult result

readInputFile :: Options -> IO Text
readInputFile Options {inputFile} = do
  bs <- B.readFile inputFile `catchIO` \_ -> throwIO $ CannotReadFile inputFile
  case (T.decodeUtf8' bs, decodeStrictByteStringExplicit CP1251 bs) of
    (Right text, _) -> pure text
    (_, Right string) -> pure $ T.pack string
    _ -> throwIO $ CannotDecodeFile inputFile

assembleProgram :: Options -> Text -> IO ByteString
assembleProgram Options {inputFile} input =
  case parseProgram inputFile input of
    Left errors -> throwIO $ CannotParseProgram errors
    Right program -> pure $ encodeProgram program

writeOutputFile :: Options -> ByteString -> IO ()
writeOutputFile Options {inputFile, outputFile} output = do
  let file = fromMaybe (replaceExtension inputFile "x584") outputFile
  B.writeFile file output `catchIO` \_ -> throwIO $ CannotWriteFile file

showAppError :: AppError -> IO ()
showAppError = die . displayException

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch
