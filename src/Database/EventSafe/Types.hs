{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.EventSafe.Types
  ( ResourceRef(..)
  , Resource(..)
  , EventPool(..)
  , StorableEvent(..)
  ) where

import Data.List
import qualified Data.ByteString.Lazy as BSL

-- | A type that can refer to a type of event.
--
-- This typeclass is used to identify a resource and filter wath events concern this specific resource.
class ResourceRef e ref where
  -- ^ A predicate used to know if an event concerns a resource.
  concerns :: e   -- ^ The event.
           -> ref -- ^ The reference to the resource.
           -> Bool

-- | A type that can be built from a specific type of event.
class Resource e res where
  -- | Build a resource from the first chronological event concerning this resource.
  firstEvent :: e         -- ^ The event.
             -> Maybe res -- ^ The resulting resource if available.
  -- | Build a resource from a previous version of this version and an event.
  -- This event will be /applied/ to the resource.
  applyEvent :: e         -- ^ The event.
             -> res       -- ^ The previous version of the resource (an accumulator).
             -> Maybe res -- ^ The resulting resource if available.

  -- | Build a resource from a list of events
  --
  -- The default implementation build a resource from the first event using 'firstEvent'
  -- and apply the others with 'applyEvent.
  -- Should any of these functions return 'Nothing', the whole process returns 'Nothing'.
  buildResource :: [e]       -- ^ The events to build the resource from.
                -> Maybe res -- ^ The resulting resource if available.
  buildResource [] = Nothing
  buildResource (fe:es) = foldl' (\mres e -> mres >>= applyEvent e) (firstEvent fe) es

-- | A structure capable of storing events.
class EventPool p e where
  -- | Filter the events concerning a resource (see 'Resource') specified by a reference (see 'ResourceRef').
  --
  -- 'filterEvent' is used to build a resource from its reference. The list of events is then passed to 'buildResource'. See 'getResource'.
  filterEvents :: ResourceRef e ref
               => p e -- ^ The pool of events.
               -> ref -- ^ The reference to the resource.
               -> [e] -- ^ The events concerning this resource.
  -- | Add an event to the pool.
  addEvent     :: p e -- ^ The pool of events.
               -> e   -- ^ The event to be added.
               -> p e -- ^ A new version of the pool with the additional event.

  -- | Get a resource from an 'EventPool'
  --
  -- The default implementation uses 'filterEvents' and 'buildResource' in order to
  -- get the events concerning a resource and build it.
  getResource  :: (ResourceRef e ref, Resource e res)
               => p e       -- ^ The pool of events.
               -> ref       -- ^ The reference to the resource to build.
               -> Maybe res -- ^ The resulting resource if available.
  getResource pool ref = buildResource $ filterEvents pool ref

-- | A type of event that can be stored on disk.
class Ord e => StorableEvent e where
  -- | Encode an event to a lazy 'ByteString'.
  encode :: e -> BSL.ByteString
  -- | Try to decode an event from a lazy 'ByteString'.
  decode :: BSL.ByteString -> Maybe e

instance Ord e => EventPool [] e where
  filterEvents pool ref = filter (flip concerns ref) pool
  addEvent = flip insert
