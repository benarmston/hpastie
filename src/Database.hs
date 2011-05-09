module Database
    ( createTableIfMissing
    , savePasteToDb
    , getPasteFromDb
    , getAllPastes
    , getAllUsedLanguages
    , getPastesForLang
    ) where

import           Control.Monad (unless)
import           Control.Monad.Trans(MonadIO, liftIO)
import           Data.Time.Clock(getCurrentTime)
import           Database.HDBC
import           Types
import           Paste


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


getAllPastes :: (MonadIO m, IConnection conn) => conn -> m [Paste]
getAllPastes db = pastesFromQuery db "SELECT * FROM pastes ORDER BY title" []


getPasteFromDb :: (Functor m, MonadIO m, IConnection conn) => conn -> Integer -> m (Maybe Paste)
getPasteFromDb db uid = do
    pasteFromQuery db "SELECT * FROM pastes WHERE id = ?" [toSql uid]


getAllUsedLanguages :: (MonadIO m, IConnection conn) => conn -> m [String]
getAllUsedLanguages db = do
    langs <- handleSqlError' $
               quickQuery db "SELECT DISTINCT syntax FROM pastes WHERE syntax != \"\" ORDER BY syntax" []
    return $ map (fromSql . (!! 0)) langs


getPastesForLang :: (MonadIO m, IConnection conn) => conn -> String -> m [Paste]
getPastesForLang db lang =
    pastesFromQuery db "SELECT * FROM pastes WHERE syntax = ? ORDER BY title" [toSql lang]


pasteFromQuery :: (Functor m, MonadIO m, IConnection conn) => conn -> String -> [SqlValue] -> m (Maybe Paste)
pasteFromQuery db sqlString sqlArgs = do
    pastes <- pastesFromQuery db sqlString sqlArgs
    case pastes of
         [] -> return Nothing
         _  -> return . Just $ pastes !! 0


pastesFromQuery :: (MonadIO m, IConnection conn) => conn -> String -> [SqlValue] -> m [Paste]
pastesFromQuery db sqlString sqlArgs = do
    pastes <- handleSqlError' $ quickQuery db sqlString sqlArgs
    return $ map makePaste pastes
  where
    makePaste ::  [SqlValue] -> Paste
    makePaste [pid, tit, ts, synt, cont] = Paste { pasteId = fromSql pid
                                                 , pasteTitle = fromSql tit
                                                 , pasteTimestamp = fromSql ts
                                                 , pasteSyntax = fromSql synt
                                                 , pasteContents = fromSql cont }
    makePaste _                          = nullPaste


handleSqlError' ::  (MonadIO m) => IO a -> m a
handleSqlError' = liftIO . handleSqlError
