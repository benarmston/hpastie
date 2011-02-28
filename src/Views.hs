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
import           Text.Highlighting.Kate (FormatOption(..), languages, highlightAs, formatAsXHtml, defaultHighlightingCss)
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
    ul ! class_ "errors" $ do
        forM_ errors (li . toHtml)
    H.form ! method "POST" $ do
        H.label "Title"
        input ! name "title" ! size "50" ! value (toValue $ pasteTitle paste)
        br
        H.label "Language"
        select ! name "syntax" ! (value $ toValue $ pasteSyntax paste) $ do
          forM_ ("":languages) (\o -> option ! value (toValue o) $ toHtml o)
        br
        textarea ! name "contents" ! rows "20" ! cols "76" $ toHtml $ pasteContents paste
        br
        input ! type_ "submit" ! value "Save"


pasteToHtml ::  Paste -> Template
pasteToHtml paste = layout "Paste" $ do
    div ! (id uid) $ do
        h2 $ pTitle paste
        p ! class_ "timestamp" $ toHtml $ "Uploaded at " ++ formattedTime
        maybeDisplaySyntax
        pre $ formattedCode
    where contents = filter (/='\r') $ pasteContents paste
          syntax = pasteSyntax paste
          formattedCode = case highlightAs syntax contents of
                               Left _ -> pre $ toHtml contents
                               Right c -> preEscapedString . show $ formatAsXHtml [OptNumberLines] syntax c
          timestamp = pasteTimestamp paste
          formattedTime = formatTime defaultTimeLocale "%F %R UTC" timestamp
          uid = toValue $ pId paste
          maybeDisplaySyntax = if syntax == "" then "" else p ! class_ "syntax" $ toHtml $ "Language " ++ syntax


layout :: String -> Html -> Template
layout page_title page_body start_time current_time = docTypeHtml $ do
    head $ do
        H.title $ toHtml page_title
        -- XXX Move outside of layout. Perhaps views can provide a page_head
        -- to include?
        H.style ! type_ "text/css" $ toHtml defaultHighlightingCss
    body $ do
        header $ do
            h1 $ a ! href "/" $ "Ben's paste bin"
        page_body
        footer $ do
            string "Config generated at " >> toHtml start_time
            string ". Page generated at " >> toHtml current_time


pTitle :: Paste -> Html
pTitle = toHtml . pasteTitle


pUrl :: Paste -> AttributeValue
pUrl paste = toValue $ "/paste/" ++ pId paste


pId :: Paste -> String
pId = show . pasteId


instance ToHtml UTCTime where
    toHtml = string . show
