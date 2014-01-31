module Database.EventSafe.DiscPoolBenchs
  ( discPoolBenchs
  ) where

import Database.EventSafe.Types
import Database.EventSafe.DiscPool

import Criterion.Main

import Examples.Shared

import System.IO.Temp

addingEventsBench :: Int -> Benchmark
addingEventsBench n = bench ("adding " ++ show n ++ " events to an empty pool") $ do
  withSystemTempDirectory "eventsafe-benchs-" $ \dir -> do
    pool <- makeDiscPool dir 3
    let event  = UserCreation (show n) (Email "email@tld.com") "pass123"
        events = replicate n event
    mapM_ (addEventM pool) events

discPoolBenchs :: Benchmark
discPoolBenchs = bgroup "DiscPool"
  [ bgroup "Adding events" $ map addingEventsBench [16, 32, 64, 128]
  ]
