{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Database.EventSafe.PoolPair
  ( PoolPair(..)
  ) where

import Database.EventSafe.Types

-- | A combinaison of two pools which is itself an 'EventPool'.
data PoolPair a b e = PoolPair
  { leftPool  :: !(a e) -- ^ The fast pool, that's the one that is going to be queried.
  , rightPool :: !(b e) -- ^ A redundant pool.
  }

instance (EventPool a e, EventPool b e) => EventPool (PoolPair a b) e where
  emptyPool             = PoolPair emptyPool emptyPool
  filterEvents pair ref = filterEvents (leftPool pair) ref
  addEvent pair event   = PoolPair
    (addEvent (leftPool pair) event)
    (addEvent (rightPool pair) event)

instance (EventPoolM m a e, EventPoolM m b e) => EventPoolM m (PoolPair a b) e where
  emptyPoolM = do
    a <- emptyPoolM
    b <- emptyPoolM
    return $ PoolPair a b

  filterEventsM pair ref = filterEventsM (leftPool pair) ref

  addEventM pair event = do
    a <- addEventM (leftPool pair)  event
    b <- addEventM (rightPool pair) event
    return $ PoolPair a b
