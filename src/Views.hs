{-# LANGUAGE OverloadedStrings #-}

module Views
    ( indexView
    , echoView
    , tablesView
    , renderHtml
    , Html
    , pasteForm
    , pasteToHtml
    ) where

import           Prelude hiding (head, div, id)
import           Control.Monad(forM_)

import           Data.Time.Clock(UTCTime)
import           Data.Time.Format(formatTime)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Utf8 (renderHtml)
import           Snap.Types()
import           System.Locale(defaultTimeLocale)

import           Types


indexView :: UTCTime -> UTCTime -> Html
indexView start_time current_time = docTypeHtml $ do
    head $ do
        H.title "Snap web server"
        link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
    body $ do
        div ! id "content" $ do
            h1 "It works!"
            p $ do
                "This is a simple demo page served using "
                a ! href "http://jaspervdj.be/blaze" $ "BlazeHtml"
                " and the "
                a ! href "http://snapframework.com/" $ "Snap"
                " web framework."
            p $ do
                "Echo test:"
                ul $ do
                    li $ a ! href "/echo/cats" $ "cats"
                    li $ a ! href "/echo/dogs" $ "dogs"
                    li $ a ! href "/echo/fish" $ "fish"
            table ! id "info" $ do
                tr $ do
                    td $ "Config generated at:"
                    td $ toHtml start_time
                tr $ do
                    td $ "Page generated at:"
                    td $ toHtml current_time


echoView :: ToHtml a => a -> Html
echoView message = docTypeHtml $ do
    head $ do
        H.title "Echo Page"
    body $ do
        div ! id ("content") $ do
            h1 "Is there an echo in here?"
        p $ "You wanted me to say this?"
        p $ toHtml message
        p $ a ! href "/" $ "Return"


tablesView :: [String] -> Html
tablesView tables = do
    head $ do
        H.title "Tables Page"
    body $ do
        div ! id ("content") $ do
            h1 "All the tables in the database"
            ul $ forM_ tables (li . toHtml)


pasteForm :: [String] -> Paste -> Html
pasteForm errors paste = layout "Paste form" $ do
    H.ul ! A.class_ "errors" $ do
        forM_ errors (H.li . H.toHtml)
    H.form ! A.method "POST" $ do
        H.label "Title"
        H.input ! A.name "title" ! A.size "50" ! A.value (H.stringValue $ pasteTitle paste)
        H.br
        H.label "Syntax"
        -- H.select ! A.name "syntax" ! (A.value $ H.stringValue $ pasteSyntax paste)
        --    forM_ [""] (\o -> H.option ! value (stringValue o) $ string o)
        H.br
        H.textarea ! A.name "contents" ! A.rows "20" ! A.cols "76" $ H.string $ pasteContents paste
        H.br
        H.input ! A.type_ "submit" ! A.value "Save"


pasteToHtml ::  Paste -> Html
pasteToHtml paste = layout "Paste" $ do
    H.div ! (A.id uid) $ do
        H.h2 title
        H.p ! A.class_ "timestamp" $ formattedTime
        H.pre $ formattedCode
    where contents = filter (/='\r') $ pasteContents paste
          title = H.string $ pasteTitle paste
          --syntax = pasteSyntax paste
          formattedCode = H.string $ contents
          timestamp = pasteTimestamp paste
          formattedTime = H.string $ formatTime defaultTimeLocale "%F %R UTC" timestamp
          uid = H.stringValue . show $ pasteId paste


layout :: String -> Html -> Html
layout title body = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.string title
    H.body $ body



instance ToHtml UTCTime where
    toHtml = string . show
