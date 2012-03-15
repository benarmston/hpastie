{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Control.Monad ((<=<))
import           Control.Monad.Trans(liftIO)
import           Data.Maybe
import           Data.Time.Clock(getCurrentTime)

import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Snap.Extension.Timer
import           Snap.Extension.HDBC
import           Snap.Util.FileServe
import           Snap.Types

import           Application
import           Database
import           Views
import           Types
import           Paste


------------------------------------------------------------------------------
-- | Render a list of all pastes
pastesListH :: Application ()
pastesListH = withDb getAllPastes >>= (renderView . pastesListV)


------------------------------------------------------------------------------
-- | Renders a form to add a new paste.
pasteFormH :: Application ()
pasteFormH = renderView $ pasteFormV [] nullPaste


------------------------------------------------------------------------------
-- | Saves a new paste to the database or redisplays the form with a list of
-- errors.
addPasteH ::  Application ()
addPasteH = do
    title <- decodeParam "title"
    contents <- decodeParam "contents"
    syntax <- decodeParam "syntax"
    let paste = nullPaste { pasteTitle = title
                          , pasteContents = contents
                          , pasteSyntax = syntax }
    let errors = validatePaste paste
    if not (null errors)
       then renderView $ pasteFormV errors paste
       else do
           uid <- withDb $ flip savePasteToDb paste
           redirect $ pack ("/paste/" ++ show uid)


------------------------------------------------------------------------------
-- | Display a single paste.
pasteH ::  Application ()
pasteH = getParam "id" >>= maybe pass readPid >>= maybe pass pasteFromId >>= maybe pass renderPasteV
    where
      readPid pid = case reads (unpack pid) of
                        [(pid', "")] -> return $ Just pid'
                        _            -> return Nothing
      renderPasteV = renderView . pasteV
      pasteFromId pid = withDb $ flip getPasteFromDb pid


------------------------------------------------------------------------------
-- | Display a list of all languages used by the pastes
languagesListH ::  Application ()
languagesListH = withDb getAllUsedLanguages >>= renderView . languagesListV


------------------------------------------------------------------------------
-- | Display a list of all pastes for the given language.
languageH ::  Application ()
languageH = do
    lang <- decodeParam "lang"
    pastes <- withDb $ flip getPastesForLang lang
    renderView $ languageV lang pastes


------------------------------------------------------------------------------
-- | Renders a BlazeHtml template and writes it to the response stream.
renderView :: Template -> Application ()
renderView template = do
    start_time <- startTime
    current_time <- liftIO getCurrentTime
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml $ template start_time current_time


------------------------------------------------------------------------------
-- | Return the value of the given parameter or an empty string if the
-- parameter doesn't exist.
decodeParam :: ByteString -> Application String
decodeParam = return . unpack . fromMaybe "" <=< getParam


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = withDb createTableIfMissing >>
       route [ ("/",          method GET  $ ifTop pastesListH)
             , ("/new",       method GET  pasteFormH)
             , ("/new",       method POST addPasteH)
             , ("/paste/:id", method GET  pasteH)
             , ("/languages", method GET  languagesListH)
             , ("/language/:lang", method GET languageH)
             ]
       <|> serveDirectory "resources/static"
