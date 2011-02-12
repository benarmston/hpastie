module Snap.Extension.HDBC
  ( MonadHDBC(..)
  ) where

-- XXX Importing Sqlite3 shouldn't be neccessary here.
-- We ought to use something like
--     dbConn :: IConnection conn => m conn
-- but I can't get that working at the moment.
import           Database.HDBC.Sqlite3
import           Snap.Types


------------------------------------------------------------------------------
class MonadSnap m => MonadHDBC m where
    dbConn :: m Connection
