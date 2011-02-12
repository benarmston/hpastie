module Database
    ( createTableIfMissing
    , listTables
    ) where

import           Control.Monad (unless)
import           Database.HDBC


createTableIfMissing ::  (IConnection conn) => conn -> IO ()
createTableIfMissing db = do
    tables <- handleSqlError $ getTables db
    unless ("pastes" `elem` tables) $ handleSqlError $ do
        run db ("CREATE TABLE pastes (id INTEGER PRIMARY KEY AUTOINCREMENT," ++
            " title TEXT, timestamp DATE, syntax VARCHAR(20), contents TEXT)") []
        commit db


listTables ::  (IConnection conn) => conn -> IO [String]
listTables = handleSqlError . getTables
