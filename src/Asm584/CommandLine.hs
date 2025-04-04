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

module Asm584.CommandLine where

import Asm584.Types
import Data.String.Interpolate (i)
import Data.Version
import Options.Applicative
import qualified Options.Applicative as Opt
import Paths_asm584 (version)

parseArguments :: [String] -> ParserResult Options
parseArguments = execParserPure defaultPrefs parserInfo

parserInfo :: ParserInfo Options
parserInfo =
  info
    (helpP <*> versionP <*> optionsP)
    ( fullDesc
        <> header "asm584 - a simple assembler for K584VM1 microprocessor"
        <> progDesc "Translate ASMFILE to .x584 file for use in X584 simulator"
    )

optionsP :: Opt.Parser Options
optionsP =
  Options
    <$> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "X584FILE"
              <> help "Write the translated program to this file"
          )
      )
    <*> strArgument
      (metavar "ASMFILE" <> help "Path to the assembly source file")

-- | Like 'simpleVersioner' but it is always visible and supports "-v" switch.
versionP :: Opt.Parser (a -> a)
versionP =
  infoOption
    [i|asm584 #{showVersion version}|]
    (long "version" <> short 'v' <> help "Show version information")

-- | Like 'helper' but it is always visible.
helpP :: Opt.Parser (a -> a)
helpP =
  abortOption
    (ShowHelpText Nothing)
    (long "help" <> short 'h' <> help "Show this help text")
