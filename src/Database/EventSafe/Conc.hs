module Database.EventSafe.Conc
  ( ESTVar
  , writeEventSTM
  , readResourceMem
  , writeEventMem
  ) where

import Control.Concurrent.STM
import Database.EventSafe.Types

type ESTVar p e = TVar (p e)

writeEventSTM :: EventPool p e => TVar (p e) -> e -> STM ()
writeEventSTM poolVar event = do
  pool <- readTVar poolVar
  writeTVar poolVar $ addEvent pool event

readResourceMem :: (ResourceRef e ref, Resource e res, EventPool p e)
                => TVar (p e) -> ref -> IO (Maybe res)
readResourceMem poolVar ref = do
  pool <- atomically $ readTVar poolVar
  return $ getResource pool ref

writeEventMem :: EventPool p e => TVar (p e) -> e -> IO ()
writeEventMem poolVar event = atomically $ writeEventSTM poolVar event
