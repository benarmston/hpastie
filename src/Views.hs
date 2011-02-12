{-# LANGUAGE OverloadedStrings #-}

module Views
    ( indexView
    , echoView
    , tablesView
    , renderHtml
    , Html
    ) where

import           Prelude hiding (head, div, id)
import           Control.Monad(forM_)

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Utf8 (renderHtml)
import           Snap.Types()
import           Data.Time.Clock(UTCTime)


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


instance ToHtml UTCTime where
    toHtml = string . show
