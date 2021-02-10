module Suem (Config(..), suem) where

data Config = Config {
    freq :: Int,
    ram  :: Int,
    rom  :: String
}

suem :: Config -> IO ()
suem (Config _ _ r) = putStrLn $ "Loaded " ++ r ++ " into ROM."
suem _              = return ()
