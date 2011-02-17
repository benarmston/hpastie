module Types
    ( Paste(..)
    , nullPaste
    ) where

import           Data.Time.Clock(UTCTime)


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
