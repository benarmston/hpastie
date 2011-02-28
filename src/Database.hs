module Database
    ( createTableIfMissing
    , savePasteToDb
    , getPasteFromDb
    , getAllPastes
    , getAllUsedLanguages
    , getPastesForLang
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


getAllPastes :: (MonadIO m, MonadPlus m, IConnection conn) => conn -> m [Paste]
getAllPastes db = do
    pastes <- liftIO $ handleSqlError $ quickQuery db "SELECT * FROM pastes ORDER BY title" []
    return $ map makePaste pastes
    where makePaste ([pid, tit, ts, synt, cont]) =
              Paste { pasteId = fromSql pid
                    , pasteTitle = fromSql tit
                    , pasteTimestamp = fromSql ts
                    , pasteSyntax = fromSql synt
                    , pasteContents = fromSql cont }
          makePaste _ = nullPaste

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


getAllUsedLanguages :: (MonadIO m, IConnection conn) => conn -> m [String]
getAllUsedLanguages db = do
    langs <- liftIO $ handleSqlError $
               quickQuery db "SELECT DISTINCT syntax FROM pastes WHERE syntax != \"\" ORDER BY syntax" []
    return $ map (fromSql . (!! 0)) langs


getPastesForLang :: (MonadIO m, MonadPlus m, IConnection conn) => conn -> String -> m [Paste]
getPastesForLang db lang = do
    pastes <- liftIO $ handleSqlError $
                quickQuery db "SELECT * FROM pastes WHERE syntax = ? ORDER BY title" [toSql lang]
    return $ map makePaste pastes
    where makePaste ([pid, tit, ts, synt, cont]) =
              Paste { pasteId = fromSql pid
                    , pasteTitle = fromSql tit
                    , pasteTimestamp = fromSql ts
                    , pasteSyntax = fromSql synt
                    , pasteContents = fromSql cont }
          makePaste _ = nullPaste
