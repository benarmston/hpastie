{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Control.Monad.Trans(liftIO)
import           Data.Maybe
import           Data.Time.Clock(getCurrentTime)

import           Data.ByteString.Char8 (pack, unpack)
import           Snap.Extension.Timer
import           Snap.Extension.HDBC
import           Snap.Util.FileServe
import           Snap.Types

import           Application
import           Database
import           Views
import           Types


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ do
    start_time <- startTime
    current_time <- liftIO getCurrentTime
    blazeTemplate $ indexView start_time current_time


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    blazeTemplate $ echoView $ show message
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | Renders a list of the tables in the database.
tables :: Application ()
tables = dbConn >>= liftIO . listTables >>= blazeTemplate . tablesView


showPasteForm :: Application ()
showPasteForm = blazeTemplate $ pasteForm [] nullPaste


addPaste ::  Application ()
addPaste = do
    let isEmpty = all (`elem` " \t")
    title <- getParam "title"
    contents <- getParam "contents"
    syntax <- getParam "syntax"
    let paste = nullPaste { pasteTitle = ( unpack $ fromMaybe "" title )
                          , pasteContents = (unpack $ fromMaybe "" contents)
                          , pasteSyntax = (unpack $ fromMaybe "" syntax) }
    let errors = ["Title must not be empty" | isEmpty (pasteTitle paste)] ++
                 ["Contents must not be empty" | isEmpty (pasteContents paste)]
    if not (null errors)
       then blazeTemplate $ pasteForm errors paste
       else do
           db <- dbConn
           uid <- savePasteToDb db paste
           redirect $ pack ("/paste/" ++ show uid)


showPaste ::  Application ()
showPaste = do
    uid <- getParam "id"
    case uid of
         -- Doesn't seem to be working.
         Nothing -> pass
         Just pid -> do
             db <- dbConn
             let pid' = read $ unpack pid
             getPasteFromDb db pid' >>= ( blazeTemplate . pasteToHtml )


blazeTemplate :: Html -> Application ()
blazeTemplate template = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml template


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = dbConn >>= (liftIO . createTableIfMissing) >>
       route [ ("/",            index)
             , ("/echo/:stuff", echo)
             , ("/tables",      tables)
             , ("/pastes",    method GET  $ showPasteForm)
             , ("/pastes",    method POST $ addPaste)
             , ("/paste/:id", method GET  $ showPaste)
             ]
       <|> serveDirectory "resources/static"
