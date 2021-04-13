-- This module parses config as passed in arguments and runs Emulator.
module Main where

import Options.Applicative
import Video


inet_socket :: Parser ConfigSocket
inet_socket = ConfigInet <$> strOption
  (  long "inet"
  <> short 'i'
  <> metavar "ADDR"
  <> help "Port for internet socket" )

unix_socket :: Parser ConfigSocket
unix_socket = ConfigUnix <$> strOption
  (  long "unix"
  <> short 'u'
  <> metavar "ADDR"
  <> help "Address for UNIX socket" )

socket :: Parser ConfigSocket
socket = inet_socket <|> unix_socket


main :: IO ()
main = video =<< execParser opts
  where
    opts = info (socket <**> helper)
      ( fullDesc
     <> progDesc "The way to output your colorful pixels from Suem"
     <> header "suem-video - Suem video device" )
