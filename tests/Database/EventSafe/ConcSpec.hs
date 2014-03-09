{-# LANGUAGE MultiParamTypeClasses #-}

module Database.EventSafe.ConcSpec
  ( spec
  ) where

import Control.Concurrent
import Control.Monad

import Database.EventSafe.Conc
import Database.EventSafe.Types

import Test.Hspec

data    EventExample       = EventExample deriving (Show, Eq, Ord)
data    ResourceRefExample = ResourceRefExample
newtype EventCount         = EventCount Int deriving (Show, Eq)
type    ESTVarExample      = ESTVar [] EventExample

instance ResourceRef EventExample ResourceRefExample where
  concerns _ _ = True

instance Resource EventExample EventCount where
  firstEvent _                = Just $ EventCount 1
  applyEvent _ (EventCount c) = Just $ EventCount $ c + 1
  buildResource []            = Nothing
  buildResource es            = Just $ EventCount $ length es

spec :: Spec
spec = do
  describe "ESTVar" $ do
    describe "emptyPoolM" $ do
      it "returns an empty pool" $ do
        pool <- emptyPoolM :: IO ESTVarExample
        c <- getResourceM pool ResourceRefExample
        c `shouldBe` (Nothing :: Maybe EventCount)

    describe "addEventM" $ do
      it "adds events concurrently to a ESTVar" $ do
        pool <- emptyPoolM :: IO ESTVarExample

        let n = 1000
        mvars <- replicateM n newEmptyMVar
        forM_ mvars $ \mvar -> forkIO $ do
          addEventM pool EventExample
          putMVar mvar True
        mapM_ takeMVar mvars

        c <- getResourceM pool ResourceRefExample
        c `shouldBe` Just (EventCount n)
