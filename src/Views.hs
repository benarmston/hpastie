{-# LANGUAGE OverloadedStrings #-}

module Views
    ( renderHtml
    , pasteForm
    , pasteToHtml
    , pasteList
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


pasteList :: [Paste] -> Template
pasteList pastes = layout "All pastes" $ do
    ul ! class_ "pastes" $ do
        forM_ pastes (li . synopsis)
    where synopsis paste = a ! href (pUrl paste) $ pTitle paste


pasteForm :: [String] -> Paste -> Template
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


pasteToHtml ::  Paste -> Template
pasteToHtml paste = layout "Paste" $ do
    H.div ! (A.id uid) $ do
        H.h2 $ pTitle paste
        H.p ! A.class_ "timestamp" $ formattedTime
        H.pre $ formattedCode
    where contents = filter (/='\r') $ pasteContents paste
          --syntax = pasteSyntax paste
          formattedCode = H.string $ contents
          timestamp = pasteTimestamp paste
          formattedTime = H.string $ formatTime defaultTimeLocale "%F %R UTC" timestamp
          uid = H.stringValue . show $ pasteId paste


layout :: String -> Html -> Template
layout page_title page_body start_time current_time = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.string page_title
    H.body $ do
        page_body
        H.div ! A.id "footer" $ do
            string "Config generated at " >> toHtml start_time
            string ". Page generated at " >> toHtml current_time


pTitle :: Paste -> Html
pTitle = toHtml . pasteTitle


pUrl :: Paste -> AttributeValue
pUrl paste = stringValue $ "/paste/" ++ (show $ pasteId paste)


instance ToHtml UTCTime where
    toHtml = string . show
