{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.EventSafe.Storage
  ( EventStorage(..)
  , newEventStorage
  , loadStorage
  ) where

import           System.Directory
import           System.FilePath

import           Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy as BSL

import           Control.Monad
import           Control.Monad.Trans

import           Database.EventSafe.Conc
import           Database.EventSafe.Types

-- | A data structure representing a storage of events on disk together with information about storage in memory.
data EventStorage p e
  -- | The first argument is a 'TVar' holding an 'EventPool' of events,
  -- the second is the directory those events should be read from / written to.
  = EventStorage (ESTVar p e) FilePath

instance (MonadIO m, EventPool p e, StorableEvent e) => EventPoolM m (EventStorage p) e where
  emptyPoolM = newEventStorage "/var/eventsafe/data"

  filterEventsM (EventStorage poolVar _) = filterEventsM poolVar

  -- Note that we do not modify the pool so we can just return it
  addEventM storage@(EventStorage poolVar path) event = do
    liftIO $ writeEventToDir path event
    _ <- addEventM poolVar event
    return storage

-- | Convenient function to create a new empty 'EventStorage'.
newEventStorage :: (MonadIO m, EventPool p e, StorableEvent e)
                => FilePath -> m (EventStorage p e)
newEventStorage path = do
  poolVar <- emptyPoolM
  return $ EventStorage poolVar path

writeEventToDir :: StorableEvent e => FilePath -> e -> IO ()
writeEventToDir dir event = do
  let encodedEvent = encode event
      md5Hash      = show $ md5 encodedEvent
      path         = dir </> md5Hash
  BSL.writeFile path encodedEvent

-- | Load events stored on disc into memory.
loadStorage :: (MonadIO m, EventPool p e, StorableEvent e) => EventStorage p e -> m ()
loadStorage (EventStorage poolVar dir) = do
  files <- liftIO $ getDirectoryContents dir
  let eventFiles = map (dir </>) $ filter ((/= '.') . head) files
  forM_ eventFiles $ \path -> do
    content <- liftIO $ BSL.readFile path
    let mevent = decode content
    case mevent of
      Nothing    -> return ()
      Just event -> addEventM poolVar event >> return ()
