{-# LANGUAGE OverloadedStrings #-}

module Views
    ( renderHtml
    , pasteFormV
    , pasteV
    , pastesListV
    , languagesListV
    , languageV
    ) where

import Prelude hiding (head, div, id)
import Control.Monad(forM_)
import Data.Maybe (fromMaybe)
import Data.Time.Format(formatTime)
import System.Locale(defaultTimeLocale)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Highlighting.Kate (FormatOption(..), languages, highlightAs, formatAsXHtml, defaultHighlightingCss)

import Types
import Views.Helpers


pastesListV :: [Paste] -> Template
pastesListV pastes = layout "All pastes" $ do
    listOfLinks "pastes" makeLink pastes
    where makeLink paste = a ! href (pUrl paste) $ pTitle paste


pasteFormV :: [String] -> Paste -> Template
pasteFormV errors paste = layout "Paste form" $ do
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


pasteV ::  Paste -> Template
pasteV paste = layoutWithHeader "Paste" css $ do
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
          maybeDisplaySyntax = if syntax == ""
                                  then ""
                                  else p ! class_ "syntax" $ "Language " >> syntaxLink
          syntaxLink = a ! href (toValue $ "/language/" ++ syntax) $ toHtml syntax
          css = inlineCss $ defaultHighlightingCss


languagesListV ::  [String] -> Template
languagesListV langs = layout "Languages" $ do
    listOfLinks "languages" makeLink langs
    where makeLink l = a ! href (langUrl l) $ toHtml l


languageV :: String -> [Paste] -> Template
languageV l pastes = layout ("Pastes for " ++ l) $ do
    p intro
    ul ! class_ "pastes" $ do
        forM_ pastes (li . synopsis)
    where synopsis paste = a ! href (pUrl paste) $ pTitle paste
          intro = toHtml $ if l == ""
                              then "All pastes with no language"
                              else "All pastes for language " ++ l



layoutWithHeader :: String -> Maybe Html -> Html -> Template
layoutWithHeader page_title page_head page_body start_time current_time = docTypeHtml $ do
    head $ do
        H.title $ page_title'
        fromMaybe "" page_head
    body $ do
        header $ do
            h1 $ a ! href "/" $ "Ben's paste bin"
            nav navLinks
            h2 page_title'
        page_body
        footer $ do
            string "Config generated at " >> toHtml start_time
            string ". Page generated at " >> toHtml current_time
    where page_title' = toHtml page_title


layout ::  String -> Html -> Template
layout page_title = layoutWithHeader page_title Nothing
