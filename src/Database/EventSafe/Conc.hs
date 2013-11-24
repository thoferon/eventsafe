{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.EventSafe.Conc
  ( ESTVar
  , writeEventSTM
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans

import Database.EventSafe.Types

newtype ESTVar p e = ESTVar (TVar (p e))

instance (MonadIO m, EventPool p e) => EventPoolM m (ESTVar p) e where
  emptyPoolM = ESTVar `liftM` liftIO (newTVarIO emptyPool)

  filterEventsM (ESTVar poolVar) ref = do
    pool <- liftIO $ atomically $ readTVar poolVar
    return $ filterEvents pool ref

  addEventM pool event = do
    liftIO $ atomically $ writeEventSTM pool event
    return pool

-- | A transaction that writes an event to a 'ESTVar' holding an 'EventPool'.
writeEventSTM :: EventPool p e => ESTVar p e -> e -> STM ()
writeEventSTM (ESTVar poolVar) event = do
  pool <- readTVar poolVar
  writeTVar poolVar $ addEvent pool event
