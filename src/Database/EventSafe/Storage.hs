{-# LANGUAGE CPP #-}

module Database.EventSafe.Storage
  ( EventStorage(..)
  , readResource
  , writeEvent
  , loadStorage
  ) where

import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy as BSL

import System.FilePath
import System.IO

import Control.Concurrent.STM

import Database.EventSafe.Conc
import Database.EventSafe.Types

data EventStorage p e
  = EventStorage (ESTVar p e) FilePath
  deriving Eq

readResource :: (ResourceRef e ref, Resource e res, EventPool p)
             => EventStorage p e -> ref -> IO (Maybe res)
readResource (EventStorage tvar _) = readResourceMem tvar

writeEvent :: (EventPool p, StorableEvent e) => EventStorage p e -> e -> IO ()
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
      path         = dir </> md5Hash
  withFile path AppendMode $ flip BSL.hPut encodedEvent

loadStorage :: EventStorage p e -> IO ()
loadStorage (EventStorage tvar _) = return ()
