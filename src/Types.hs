module Types
    ( Template
    , Paste(..)
    ) where

import           Data.Time.Clock(UTCTime)
import           Text.Blaze.Html5 (Html)


------------------------------------------------------------------------------
-- | 'Template' is the type returned by the view templates. It is a function
-- taking the parameters required to complete the rendering of the layout and
-- returning Html from Blaze.
type Template = UTCTime -> UTCTime -> Html


------------------------------------------------------------------------------
data Paste = Paste { pasteId :: Integer         -- ^ The primary key.
                   , pasteTitle :: String       -- ^ The paste's title.
                   , pasteTimestamp :: UTCTime  -- ^ The creation date of the paste.
                   , pasteSyntax :: String      -- ^ The syntax to use for syntax highlighting. Any syntax supported by 'Text.Highlighting.Kate'.
                   , pasteContents :: String }
