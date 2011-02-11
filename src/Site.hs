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
import qualified Data.ByteString.Char8 as B
import           Data.Time.Clock(getCurrentTime)
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types

import           Application


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ do
    writeBS "<html><head><title>Snap web server</title>"
    writeBS "<link rel=\"stylesheet\"type=\"text/css\"href=\"screen.css\"/>"
    writeBS "</head>"
    writeBS "<body>"
    writeBS "<div id=\"content\">"
    writeBS "<h1>It works!</h1>"
    writeBS "<p>"
    writeBS "This is a simple demo page served using "
    writeBS "<a href=\"http://snapframework.com/docs/tutorials/heist\">Heist</a> "
    writeBS "and the <a href=\"http://snapframework.com/\">Snap</a> web framework."
    writeBS "</p>"
    writeBS "<p>"
    writeBS "Echo test:"
    writeBS "<a href=\"/echo/cats\">cats</a>"
    writeBS "<a href=\"/echo/dogs\">dogs</a>"
    writeBS "<a href=\"/echo/fish\">fish</a>"
    writeBS "</p>"
    writeBS "<table id=\"info\">"
    writeBS "<tr>"
    writeBS "<td>Config generated at:</td>"
    writeBS "<td>"
    start_time <- startTime
    writeBS $ B.pack $ show start_time
    writeBS "</td>"
    writeBS "</tr>"
    writeBS "<tr>"
    writeBS "<td>Page generated at:</td>"
    writeBS "<td>"
    current_time <- liftIO getCurrentTime
    writeBS $ B.pack $ show current_time
    writeBS "</td>"
    writeBS "</tr>"
    writeBS "</table>"
    writeBS "</div>"
    writeBS "</body>"
    writeBS "</html>"


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    writeBS "<html><head><title>Echo Page</title></head>"
    writeBS "<body><div id=\"content\"><h1>Is there an echo in here?</h1></div>"
    writeBS "<p>You wanted me to say this?</p>"
    writeBS "<p>"
    writeBS message
    writeBS "</p>"
    writeBS "<p><a href=\"/\">Return</a></p>"
    writeBS "</body></html>"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             ]
       <|> serveDirectory "resources/static"
