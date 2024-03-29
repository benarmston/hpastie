{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Application
  ( Application
  , applicationInitializer
  ) where

import Control.Monad.Trans (liftIO)

import           Snap.Extension
import           Snap.Extension.Timer.Impl
import           Snap.Extension.HDBC.Sqlite

import           Config

------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the HDBC and Timer Snap extensions.
type Application = SnapExtend ApplicationState


------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by the
-- Snap extensions we're using.  We're using Timer simply to illustrate the
-- config loading differences between development and production modes, and
-- we're using HDBC to provide database connectivity.
data ApplicationState = ApplicationState
    { timerState    :: TimerState
    , dbState       :: HDBCState
    }


------------------------------------------------------------------------------
instance HasTimerState ApplicationState where
    getTimerState     = timerState
    setTimerState s a = a { timerState = s }


------------------------------------------------------------------------------
instance HasHDBCState ApplicationState where
    getHDBCState     = dbState
    setHDBCState s a = a { dbState = s }


------------------------------------------------------------------------------
-- | The 'Initializer' for ApplicationState. For more on 'Initializer's, see
-- the documentation from the snap package. Briefly, this is used to
-- generate the 'ApplicationState' needed for our application and will
-- automatically generate reload\/cleanup actions for us which we don't need
-- to worry about.
applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    conf <- liftIO $ getConfig "hpastie.cfg"
    timer <- timerInitializer
    conn <- hdbcInitializer $ configDbFile conf
    return $ ApplicationState timer conn
