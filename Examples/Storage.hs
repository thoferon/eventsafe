module Examples.Storage where

-- Naive simple program prompting the user allowing him/her
-- to add new events and get resources
-- (horrible piece of code but it's just an example)

import Control.Concurrent.STM
import System.Directory
import System.IO
import Data.Time
import Data.String
import Control.Applicative

import Database.EventSafe
import Examples.Shared

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr "Storage location: "
  dir <- getLine
  createDirectoryIfMissing True dir

  storage <- newEventStorage dir
  loadStorage storage

  handleQuery storage

handleQuery :: EventStorage [] Event -> IO ()
handleQuery storage = do
  putStr "(1) New event\n(2) Get resource\nYour choice: "
  choice <- getLine
  case choice of
    "1" -> newEvent storage
    "2" -> getRes storage
    _   -> putStrLn "Unrecognized sequence."
  handleQuery storage

newEvent :: EventStorage [] Event -> IO ()
newEvent storage = do
  putStr "(1) New user\n(2) Change password\n(3) New post\nYour choice: "
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Email: "
      email <- getLine
      putStr "Password: "
      pwd <- getLine
      time <- show <$> getCurrentTime
      addEventM storage $ UserCreation time (Email email) pwd
      putStrLn "User created."
    "2" -> do
      putStr "Email: "
      email <- getLine
      putStr "Password: "
      pwd <- getLine
      time <- show <$> getCurrentTime
      addEventM storage $ UserChangePassword time (Email email) pwd
      putStrLn "Password changed."
    "3" -> do
      putStr "Id (number please): "
      postIdStr <- getLine
      putStr "Author e-mail: "
      email <- getLine
      putStr "Title: "
      title <- getLine
      time <- show <$> getCurrentTime
      addEventM storage $ PostCreation time (PostId . read $ postIdStr) (Email email) title
      putStrLn "Post created."
    _ -> putStrLn "Unrecognized sequence."

getRes :: EventStorage [] Event -> IO ()
getRes events = do
  putStr "(1) Get a user\n(2) Get a post\nYour choice: "
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Email: "
      email <- getLine
      mres <- getResourceM events (Email email) :: IO (Maybe User)
      print mres
    "2" -> do
      putStr "Post Id (number please): "
      postIdStr <- getLine
      mres <- getResourceM events (PostId . read $ postIdStr) :: IO (Maybe Post)
      print mres
    _ -> putStrLn "Unrecognized sequence."
