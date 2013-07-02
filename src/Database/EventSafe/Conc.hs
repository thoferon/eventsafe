module Database.EventSafe.Conc
  ( ESTVar
  , writeEventSTM
  , readResourceMem
  , writeEventMem
  ) where

import Control.Concurrent.STM
import Database.EventSafe.Types

type ESTVar p e = TVar (p e)

-- | A transaction that writes an event to a 'TVar' holding an 'EventPool'.
writeEventSTM :: EventPool p e => TVar (p e) -> e -> STM ()
writeEventSTM poolVar event = do
  pool <- readTVar poolVar
  writeTVar poolVar $ addEvent pool event

-- | Read a resource from an 'TVar' holding an 'EventPool'.
readResourceMem :: (ResourceRef e ref, Resource e res, EventPool p e)
                => TVar (p e) -> ref -> IO (Maybe res)
readResourceMem poolVar ref = do
  pool <- atomically $ readTVar poolVar
  return $ getResource pool ref

-- | Write an event to a 'TVar' holding an 'EventPool'.
writeEventMem :: EventPool p e => TVar (p e) -> e -> IO ()
writeEventMem poolVar event = atomically $ writeEventSTM poolVar event
