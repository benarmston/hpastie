module Paste
    ( nullPaste
    , validatePaste
    ) where

import Types


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


------------------------------------------------------------------------------
-- | Return a list of validation errors for the given 'Paste'.
--
-- Return an empty list if the 'Paste' is valid.
--
validatePaste ::  Paste -> [String]
validatePaste paste =
    ["Title must not be empty" | isEmpty (pasteTitle paste)] ++
    ["Contents must not be empty" | isEmpty (pasteContents paste)]
  where isEmpty = all (`elem` " \t")
