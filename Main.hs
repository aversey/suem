-- This module parses config as passed in arguments and runs Emulator.
module Main where

import Options.Applicative
import Suem


inet_socket :: String -> Parser ConfigSocket
inet_socket sock = ConfigInet <$> strOption
  (  long ("i" ++ sock)
  <> metavar ("ADDR_" ++ sock)
  <> help ("Port for internet socket " ++ sock) )

unix_socket :: String -> Parser ConfigSocket
unix_socket sock = ConfigUnix <$> strOption
  (  long ("u" ++ sock)
  <> metavar ("ADDR_" ++ sock)
  <> help ("Address for UNIX socket " ++ sock) )

socket :: String -> Parser (Maybe ConfigSocket)
socket sock = optional (inet_socket sock <|> unix_socket sock)


config :: Parser Config
config = Config
      <$> option auto
          ( long "frequency"
         <> short 'f'
         <> metavar "HERTZ"
         <> showDefault
         <> value 8000000
         <> help "Machine frequency in Hz" )
      <*> option auto
          ( long "ram"
         <> short 'm'
         <> metavar "RAM_SIZE"
         <> showDefault
         <> value (8 * 1024 * 1024 - 128 * 1024)
         <> help "Available RAM in bytes" )
      <*> strOption
          ( long "rom"
         <> short 'r'
         <> metavar "ROM_PATH"
         <> showDefault
         <> value "rom.bin"
         <> help "Path to file to load into ROM" )
      <*> socket "0" <*> socket "1" <*> socket "2" <*> socket "3"
      <*> socket "4" <*> socket "5" <*> socket "6" <*> socket "7"

main :: IO ()
main = suem =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Emulator of Suen, machine on M68000"
     <> header "suem - Suen emulator" )
