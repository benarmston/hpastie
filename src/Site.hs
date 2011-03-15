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


------------------------------------------------------------------------------
-- | Render a list of all pastes
showPasteList :: Application ()
showPasteList = withDb getAllPastes >>= (blazeTemplate . pasteList)


------------------------------------------------------------------------------
-- | Renders a form to add a new paste.
showPasteForm :: Application ()
showPasteForm = blazeTemplate $ pasteForm [] nullPaste


------------------------------------------------------------------------------
-- | Saves a new paste to the database or redisplays the form with a list of
-- errors.
addPaste ::  Application ()
addPaste = do
    title <- decodeParam "title"
    contents <- decodeParam "contents"
    syntax <- decodeParam "syntax"
    let paste = nullPaste { pasteTitle = title
                          , pasteContents = contents
                          , pasteSyntax = syntax }
    let errors = ["Title must not be empty" | isEmpty (pasteTitle paste)] ++
                 ["Contents must not be empty" | isEmpty (pasteContents paste)]
    if not (null errors)
       then blazeTemplate $ pasteForm errors paste
       else do
           uid <- withDb $ flip savePasteToDb paste
           redirect $ pack ("/paste/" ++ show uid)
    where isEmpty = all (`elem` " \t")


------------------------------------------------------------------------------
-- | Display a single paste.
showPaste ::  Application ()
showPaste = maybe pass showPaste' =<< getParam "id"
    where
      showPaste' pid = pasteFromId pid >>= ( blazeTemplate . pasteToHtml )
      pasteFromId pid = withDb $ flip getPasteFromDb . read . unpack $ pid


------------------------------------------------------------------------------
-- | Display a list of all languages used by the pastes
showLanguageList ::  Application ()
showLanguageList = withDb getAllUsedLanguages >>= (blazeTemplate . languageList)


------------------------------------------------------------------------------
-- | Display a list of all pastes for the given language.
showLanguage ::  Application ()
showLanguage = do
    lang <- decodeParam "lang"
    pastes <- withDb $ flip getPastesForLang lang
    blazeTemplate $ languageToHtml lang pastes


------------------------------------------------------------------------------
-- | Renders a BlazeHtml template and writes it to the response stream.
blazeTemplate :: Template -> Application ()
blazeTemplate template = do
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
       route [ ("/",          method GET  $ ifTop $ showPasteList)
             , ("/new",       method GET  $ showPasteForm)
             , ("/new",       method POST $ addPaste)
             , ("/paste/:id", method GET  $ showPaste)
             , ("/languages", method GET  $ showLanguageList)
             , ("/language/:lang", method GET $ showLanguage)
             ]
       <|> serveDirectory "resources/static"
