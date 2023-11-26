{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module parse the command line for the parameters
module CmdLine
  ( CmdOptions (..),
    cmdOptions,
  )
where

import Data.Version (showVersion)
import Options.Applicative
import Paths_hsprjup (version)

-- | put command line options here
-- | following are just for examples
-- | you must put your own cmd line options
-- | and change the parsing logic
data CmdOptions = CmdOptions
  { cmdHost :: !Text,
    cmdPort :: !Int
  }
  deriving stock (Show)

versionOptionParser :: Parser (a -> a)
versionOptionParser =
  infoOption
    (showVersion version)
    (long "version" <> short 'v' <> help "Show version")

cmdOptionsParser :: Parser CmdOptions
cmdOptionsParser =
  CmdOptions
    <$> strOption
      ( long "host"
          <> short 'm'
          <> metavar "HOST"
          <> value "localhost"
          <> showDefault
          <> help "The hostname/IP/DNS name of the target."
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 9060
          <> showDefault
          <> help "The port number of the target."
      )

cmdOptions :: ParserInfo CmdOptions
cmdOptions =
  info
    (cmdOptionsParser <**> helper <**> versionOptionParser)
    ( fullDesc
        <> progDesc "cook haskell project based on template."
        <> header ("hsprjup " <> showVersion version <> " - cook haskell project based on template.")
    )
