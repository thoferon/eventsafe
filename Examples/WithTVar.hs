{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.WithTVar where

import Control.Concurrent.STM
import Control.Applicative
import Data.Time
import System.IO

import Database.EventSafe
import Examples.Shared

-- Naive simple program prompting the user allowing him/her
-- to add new events and get resources
-- (horrible piece of code but it's just an example)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  events <- newTVarIO []
  handleQuery events

handleQuery :: TVar [Event] -> IO ()
handleQuery events = do
  putStr "(1) New event\n(2) Get resource\nYour choice: "
  choice <- getLine
  case choice of
    "1" -> newEvent events
    "2" -> getRes events
    _   -> putStrLn "Unrecognized sequence."
  handleQuery events

newEvent :: TVar [Event] -> IO ()
newEvent events = do
  putStr "(1) New user\n(2) Change password\n(3) New post\nYour choice: "
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Email: "
      email <- getLine
      putStr "Password: "
      pwd <- getLine
      time <- show <$> getCurrentTime
      writeEventMem events $ UserCreation time (Email email) pwd
      putStrLn "User created."
    "2" -> do
      putStr "Email: "
      email <- getLine
      putStr "Password: "
      pwd <- getLine
      time <- show <$> getCurrentTime
      writeEventMem events $ UserChangePassword time (Email email) pwd
      putStrLn "Password changed."
    "3" -> do
      putStr "Id (number please): "
      postIdStr <- getLine
      putStr "Author e-mail: "
      email <- getLine
      putStr "Title: "
      title <- getLine
      time <- show <$> getCurrentTime
      writeEventMem events $ PostCreation time (PostId . read $ postIdStr) (Email email) title
      putStrLn "Post created."
    _ -> putStrLn "Unrecognized sequence."

getRes :: TVar [Event] -> IO ()
getRes events = do
  putStr "(1) Get a user\n(2) Get a post\nYour choice: "
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Email: "
      email <- getLine
      mres <- readResourceMem events (Email email) :: IO (Maybe User)
      print mres
    "2" -> do
      putStr "Post Id (number please): "
      postIdStr <- getLine
      mres <- readResourceMem events (PostId . read $ postIdStr) :: IO (Maybe Post)
      print mres
    _ -> putStrLn "Unrecognized sequence."
