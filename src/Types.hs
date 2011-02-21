module Types
    ( Template
    , Paste(..)
    , nullPaste
    ) where

import           Data.Time.Clock(UTCTime)
import           Text.Blaze.Html5 (Html)

type Template = UTCTime -> UTCTime -> Html

data Paste = Paste { pasteId :: Integer
                   , pasteTitle :: String
                   , pasteTimestamp :: UTCTime
                   , pasteSyntax :: String
                   , pasteContents :: String }


nullPaste :: Paste
nullPaste = Paste  { pasteId = undefined
                   , pasteTitle = ""
                   , pasteTimestamp = undefined
                   , pasteSyntax = ""
                   , pasteContents = ""}
