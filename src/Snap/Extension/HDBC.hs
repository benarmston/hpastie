{-# LANGUAGE RankNTypes #-}

module Snap.Extension.HDBC
  ( MonadHDBC(..)
  ) where

import Control.Monad.Trans
import Database.HDBC


------------------------------------------------------------------------------
class MonadIO m => MonadHDBC m where
    dbConn :: m ConnWrapper
    withDb :: (forall c. IConnection c => c -> IO b) -> m b
    withDb query = dbConn >>= liftIO . query
