{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Template
    , Paste(..)
    , nullPaste
    ) where

import           Data.ByteString (ByteString)
import           Data.Time.Clock(UTCTime)
import           Text.Blaze.Html5 (Html)


------------------------------------------------------------------------------
-- | 'Template' is the type returned by the view templates. It is a function
-- taking the parameters required to complete the rendering of the layout and
-- returning Html from Blaze.
type Template = UTCTime -> UTCTime -> Html


------------------------------------------------------------------------------
data Paste = Paste { pasteId :: Integer         -- ^ The primary key.
                   , pasteTitle :: ByteString       -- ^ The paste's title.
                   , pasteTimestamp :: UTCTime  -- ^ The creation date of the paste.
                   , pasteSyntax :: ByteString      -- ^ The syntax to use for syntax highlighting. Any syntax supported by 'Text.Highlighting.Kate'.
                   , pasteContents :: ByteString }


------------------------------------------------------------------------------
-- | An empty 'Paste'. Useful for quickly creating a new paste, e.g.,
--
-- > nullPaste { pasteTitle = theTitle }
nullPaste :: Paste
nullPaste = Paste  { pasteId = undefined
                   , pasteTitle = ""
                   , pasteTimestamp = undefined
                   , pasteSyntax = ""
                   , pasteContents = ""}
