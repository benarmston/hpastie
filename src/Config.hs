module Config
    ( getConfig
    , Config(..)
    ) where

import Data.ConfigFile

import Types

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        dbfile <- get c "DATABASE" "dbfile"
        return Config { configDbFile = dbfile }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config
