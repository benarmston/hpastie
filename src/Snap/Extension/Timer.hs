{-|

'Snap.Extension.Timer' exports the 'MonadTimer' interface which allows you to
keep track of the time at which your application was started. The interface's
only operation is 'startTime'.

'Snap.Extension.Timer.Impl' contains the only implementation of this interface
and can be used to turn your application's monad into a 'MonadTimer'.

More than anything else, this is intended to serve as an example Snap
Extension to any developer wishing to write their own Snap Extension.

-}

module Snap.Extension.Timer
  ( MonadTimer(..)
  ) where

import           Data.Time.Clock
import           Snap.Types


------------------------------------------------------------------------------
-- | The 'MonadTimer' type class. Minimal complete definition: 'startTime'.
class MonadSnap m => MonadTimer m where
    -- | The time at which your application was last loaded.
    startTime :: m UTCTime
