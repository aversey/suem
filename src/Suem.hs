module Suem (Config(..), ConfigSocket(..), suem) where

data ConfigSocket = ConfigInet String | ConfigUnix String

data Config = Config {
    freq :: Int,
    ram  :: Int,
    rom  :: String,
    s0   :: Maybe ConfigSocket,
    s1   :: Maybe ConfigSocket,
    s2   :: Maybe ConfigSocket,
    s3   :: Maybe ConfigSocket,
    s4   :: Maybe ConfigSocket,
    s5   :: Maybe ConfigSocket,
    s6   :: Maybe ConfigSocket,
    s7   :: Maybe ConfigSocket
}

suem :: Config -> IO ()
suem (Config _ _ r _ _ _ _ _ _ _ _) = putStrLn $ "Loaded " ++ r ++ " into ROM."
suem _                              = return ()
