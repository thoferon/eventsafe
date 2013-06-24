module Database.EventSafe
  -- * Types and typeclasses
  ( ResourceRef(..)
  , Resource(..)
  , EventPool(..)
  -- * IO actions to deal with concurrency
  , readResource
  , writeEvent
  ) where

import Control.Concurrent.STM

import Database.EventSafe.Types

readResource :: (ResourceRef e ref, Resource e res, EventPool p)
             => TVar (p e) -> ref -> IO (Maybe res)
readResource poolVar ref = do
  pool <- atomically $ readTVar poolVar
  return $ getResource pool ref

writeEvent :: EventPool p => TVar (p e) -> e -> IO ()
writeEvent poolVar event = atomically $ do
  pool <- readTVar poolVar
  writeTVar poolVar $ addEvent pool event
