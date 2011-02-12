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

import           Snap.Extension.Timer
import           Snap.Extension.HDBC
import           Snap.Util.FileServe
import           Snap.Types

import           Application
import           Database
import           Views


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
             ]
       <|> serveDirectory "resources/static"
