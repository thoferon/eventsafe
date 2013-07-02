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

-- * Typeclasses

class ResourceRef e ref where
  concerns :: e -> ref -> Bool

class Resource e res where
  firstEvent    :: e -> Maybe res
  applyEvent    :: e -> res -> Maybe res

  buildResource :: [e] -> Maybe res
  buildResource [] = Nothing
  buildResource (fe:es) = foldl' (\mres e -> mres >>= applyEvent e) (firstEvent fe) es

class EventPool p e where
  filterEvents :: ResourceRef e ref => p e -> ref -> [e]
  addEvent     :: p e -> e -> p e

  getResource  :: (ResourceRef e ref, Resource e res) => p e -> ref -> Maybe res
  getResource pool ref = buildResource $ filterEvents pool ref

class Ord e => StorableEvent e where
  encode :: e -> BSL.ByteString
  decode :: BSL.ByteString -> Maybe e

-- * Instances

instance Ord e => EventPool [] e where
  filterEvents pool ref = filter (flip concerns ref) pool
  addEvent = flip insert
