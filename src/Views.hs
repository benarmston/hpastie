{-# LANGUAGE OverloadedStrings #-}

module Views
    ( renderHtml
    , pasteForm
    , pasteToHtml
    , pasteList
    , languageList
    , languageToHtml
    ) where

import           Prelude hiding (head, div, id)
import           Control.Monad(forM_)

import           Data.List(intersperse)
import           Data.Time.Clock(UTCTime)
import           Data.Time.Format(formatTime)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Utf8 (renderHtml)
import           Text.Highlighting.Kate (FormatOption(..), languages, highlightAs, formatAsXHtml, defaultHighlightingCss)
import           Snap.Types()
import           System.Locale(defaultTimeLocale)

import           Types
import Data.Maybe(fromMaybe)

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
pasteToHtml paste = layoutWithHeader "Paste" css $ do
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


languageList ::  [String] -> Template
languageList langs = layout "Languages" $ do
    ul ! class_ "languages" $ do
        forM_ langs (li . link_to_lang)
    where link_to_lang l = a ! href (url l) $ toHtml l
          url l = toValue $ "/language/" ++ l


languageToHtml :: String -> [Paste] -> Template
languageToHtml l pastes = layout ("Pastes for " ++ l) $ do
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
        H.title $ toHtml page_title
        fromMaybe "" page_head
    body $ do
        header $ do
            h1 $ a ! href "/" $ "Ben's paste bin"
            nav $
                mapM_ (\x -> x) navLinks
        page_body
        footer $ do
            string "Config generated at " >> toHtml start_time
            string ". Page generated at " >> toHtml current_time


layout ::  String -> Html -> Template
layout page_title = layoutWithHeader page_title Nothing


navLinks ::  [Html]
navLinks = intersperse " | " links
    where links = [ a ! href "/"          $ "All pastes"
                  , a ! href "/languages" $ "All used languages"
                  , a ! href "/new"       $ "Add new paste"
                  ]


pTitle :: Paste -> Html
pTitle = toHtml . pasteTitle


pUrl :: Paste -> AttributeValue
pUrl paste = toValue $ "/paste/" ++ pId paste


pId :: Paste -> String
pId = show . pasteId


inlineCss ::  String -> Maybe Html
inlineCss = Just . ( H.style ! type_ "text/css" ) . toHtml

instance ToHtml UTCTime where
    toHtml = string . show
