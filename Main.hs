module Main where

import Options.Applicative
import Suem

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
         <> value (8 * 1024 * 1024)
         <> help "Available RAM in bytes" )
      <*> strOption
          ( long "rom"
         <> short 'r'
         <> metavar "ROM_PATH"
         <> showDefault
         <> value "rom.bin"
         <> help "Path to file to load into ROM" )

main :: IO ()
main = suem =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Emulator of Suen, machine on M68000"
     <> header "suem - Suen emulator" )
