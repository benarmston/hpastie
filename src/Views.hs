{-# LANGUAGE OverloadedStrings #-}

module Views
    ( renderHtml
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


layout :: String -> Html -> Template
layout title body start_time current_time = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.string title
    H.body $ do
        body
        H.div ! A.id "footer" $ do
            string "Config generated at " >> toHtml start_time
            string ". Page generated at " >> toHtml current_time



instance ToHtml UTCTime where
    toHtml = string . show
