module Database
    ( createTableIfMissing
    , savePasteToDb
    , getPasteFromDb
    ) where

import           Control.Monad (unless, MonadPlus, mzero)
import           Control.Monad.Trans(MonadIO, liftIO)
import           Data.Time.Clock(getCurrentTime)
import           Database.HDBC
import           Types


createTableIfMissing ::  (IConnection conn) => conn -> IO ()
createTableIfMissing db = do
    tables <- handleSqlError $ getTables db
    unless ("pastes" `elem` tables) $ handleSqlError $ do
        run db ("CREATE TABLE pastes (id INTEGER PRIMARY KEY AUTOINCREMENT," ++
            " title TEXT, timestamp DATE, syntax VARCHAR(20), contents TEXT)") []
        commit db


savePasteToDb :: (IConnection conn, MonadIO m) => conn -> Paste -> m Integer
savePasteToDb db paste = do
    let query = "INSERT INTO pastes(title, timestamp, syntax, contents)" ++
                "VALUES(?,?,?,?)"
    t <- liftIO getCurrentTime
    let vals = [toSql (pasteTitle paste), toSql t, toSql (pasteSyntax paste),
                toSql (pasteContents paste)]
    liftIO $ withTransaction db $ \d -> run d query vals
    [[uid]] <- liftIO $ quickQuery db "SELECT last_insert_rowid()" []
    return (fromSql uid)


getPasteFromDb :: (MonadIO m, MonadPlus m, IConnection conn) => conn -> Integer -> m Paste
getPasteFromDb db uid = do
    pastes <- liftIO $ handleSqlError $
                quickQuery db "SELECT * FROM pastes WHERE id = ?" [toSql uid]
    case pastes of
         ([_, tit, ts, synt, cont]:_) ->
             return Paste { pasteId = uid
                          , pasteTitle = fromSql tit
                          , pasteTimestamp = fromSql ts
                          , pasteSyntax = fromSql synt
                          , pasteContents = fromSql cont }
         _ -> mzero
