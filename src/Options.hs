{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-} 
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Options (Options
               ,options
               ,optionsConf) where

import System.Console.CmdArgs

data Options = Options
  { conf      :: FilePath
  } deriving (Show,Data,Typeable)

options = Options
  { conf = def &= opt "hpastie.cfg" &= help "The config file."
  }
  &= summary "hpastie paste bin (C) Ben Armston 2011"
  &= help "Runs a paste bin web app based on the provided configuration file."

optionsConf = conf
