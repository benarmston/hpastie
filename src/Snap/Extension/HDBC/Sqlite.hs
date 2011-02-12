{-# LANGUAGE OverloadedStrings #-}

module Snap.Extension.HDBC.Sqlite
  ( HDBCState
  , HasHDBCState(..)
  , hdbcInitializer
  , module Snap.Extension.HDBC
  ) where

import           Control.Monad.Reader

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Snap.Extension
import           Snap.Extension.HDBC
import           Snap.Types


newtype HDBCState = HDBCState
    { _dbConn :: Connection
    }

class HasHDBCState s where
    getHDBCState :: s -> HDBCState
    setHDBCState :: HDBCState -> s -> s

hdbcInitializer :: Initializer HDBCState
hdbcInitializer = do
    db <- liftIO $ handleSqlError $ connectSqlite3 "pastes.sql"
    mkInitializer $ HDBCState db


------------------------------------------------------------------------------
instance InitializerState HDBCState where
    extensionId           = const "HDBC/Sqlite"
    mkCleanup(HDBCState conn) = disconnect conn
    mkReload              = const $ return ()
    --mkReload (HDBCState conn) = do
    --    disconnect conn
    --    either error (\c -> setHDBCState (HDBCState c)) =<<
    --        connectSqlite3 "pastes.sql"


------------------------------------------------------------------------------
instance HasHDBCState s => MonadHDBC (SnapExtend s) where
    dbConn = fmap _dbConn $ asks getHDBCState


------------------------------------------------------------------------------
instance (MonadSnap m, HasHDBCState s) => MonadHDBC (ReaderT s m) where
    dbConn = fmap _dbConn $ asks getHDBCState
