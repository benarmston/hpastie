{-# LANGUAGE OverloadedStrings #-}

{-|

Utility functions used in the construction of the "Views".
-}

module Views.Helpers
    ( listOfLinks
    , pTitle
    , pUrl
    , pId
    , langUrl
    , navLinks
    , inlineCss
    ) where

import Control.Monad (forM_)
import Data.Monoid (mconcat)
import Data.List (intersperse)
import Data.Time.Clock (UTCTime)

import Snap.Types()
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Types


------------------------------------------------------------------------------
-- | Returns a UL, with class 'cls', containing links generated by calling 'linker' on each 'linkee'.
--
listOfLinks ::  AttributeValue -> (a -> Html) -> [a] -> Html
listOfLinks cls linker linkees =
    ul ! class_ cls $ do
        forM_ linkees (li . linker)


------------------------------------------------------------------------------
-- | A paste's title as Html.
--
pTitle :: Paste -> Html
pTitle = toHtml . pasteTitle


------------------------------------------------------------------------------
-- | Return the URL for the given paste.
--
pUrl :: Paste -> AttributeValue
pUrl paste = toValue $ "/paste/" ++ pId paste


------------------------------------------------------------------------------
-- | Return the paste's id as a string.
--
pId :: Paste -> String
pId = show . pasteId


------------------------------------------------------------------------------
-- | Return the URL for the given language.
--
langUrl :: String -> AttributeValue
langUrl l = toValue $ "/language/" ++ l


------------------------------------------------------------------------------
-- | Return 'Html' for the navigation links.
--
navLinks ::  Html
navLinks = mconcat $ intersperse " | " links
    where links = [ a ! href "/"          $ "All pastes"
                  , a ! href "/languages" $ "All languages"
                  , a ! href "/new"       $ "Add paste"
                  ]


------------------------------------------------------------------------------
-- | Return a style tag containing the given CSS.
--
inlineCss ::  String -> Maybe Html
inlineCss = Just . ( H.style ! type_ "text/css" ) . toHtml


instance ToHtml UTCTime where
    toHtml = string . show
