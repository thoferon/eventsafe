{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.EventSafe.Storage
  ( EventStorage(..)
  , readResource
  , writeEvent
  , loadStorage
  ) where

import Prelude hiding (FilePath, readFile)

import Data.Digest.Pure.MD5 (md5)
import Data.String (IsString(..))
import qualified Data.ByteString.Lazy as BSL

import Filesystem
import Filesystem.Path.CurrentOS hiding (encode, decode)

import Control.Monad

import Database.EventSafe.Conc
import Database.EventSafe.Types

-- | A data structure representing a storage of events on disk together with information about storage in memory.
data EventStorage p e
  -- | The first argument is a 'TVar' holding an 'EventPool' of events,
  -- the second is the directory those events should be read from / written to.
  = EventStorage (ESTVar p e) FilePath
  deriving Eq

-- | Read a resource corresponding to a specific reference from an 'EventStorage'.
readResource :: (ResourceRef e ref, Resource e res, EventPool p e)
             => EventStorage p e -> ref -> IO (Maybe res)
readResource (EventStorage tvar _) = readResourceMem tvar

-- | Write an event to an 'EventStorage'.
--
-- The event will be written in memory AND on disk.
writeEvent :: (EventPool p e, StorableEvent e) => EventStorage p e -> e -> IO ()
writeEvent (EventStorage tvar path) event = do
#ifdef __GLASGOW_HASKELL__
  -- TODO: make it a transaction with unsafeIOToSTM
  writeEventToDir path event
  writeEventMem tvar event
#else
  writeEventToDir path event
  writeEventMem tvar event
#endif

writeEventToDir :: StorableEvent e => FilePath -> e -> IO ()
writeEventToDir dir event = do
  let encodedEvent = encode event
      md5Hash      = show $ md5 encodedEvent
      path         = dir </> fromString md5Hash
  withFile path AppendMode $ flip BSL.hPut encodedEvent

-- | Load events stored on disk into memory.
loadStorage :: (EventPool p e, StorableEvent e) => EventStorage p e -> IO ()
loadStorage (EventStorage tvar dir) = do
  eventFiles <- listDirectory dir
  forM_ eventFiles $ \path -> do
    content <- readFile path
    let lazyContent = BSL.fromChunks [content]
        mevent      = decode lazyContent
    case mevent of
      Nothing    -> return ()
      Just event -> writeEventMem tvar event
