{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.WithTVar where

import Database.EventSafe
import Control.Concurrent.STM
import System.IO

newtype Email = Email String deriving (Show, Eq)

data User = User {
  userEmail       :: Email
  , userPassword  :: String
  , userPostCount :: Int
} deriving (Show, Eq)

newtype PostId = PostId Int deriving (Show, Eq)

data Post = Post {
  postId            :: PostId
  , postAuthorEmail :: Email
  , postTitle       :: String
} deriving (Show, Eq)

data Event
  = UserCreation Email String
  | UserChangePassword Email String
  | PostCreation PostId Email String
  deriving (Show, Eq)

instance ResourceRef Event Email where
  event `concerns` email = case event of
    UserCreation       email' _ -> email == email'
    UserChangePassword email' _ -> email == email'
    PostCreation     _ email' _ -> email == email'

instance ResourceRef Event PostId where
  event `concerns` pid = case event of
    PostCreation pid' _ _ -> pid == pid'
    _                        -> False

instance Resource Event User where
  firstEvent event = case event of
    UserCreation email password -> Just $ User email password 0
    _                           -> Nothing

  applyEvent event user = case event of
    UserCreation _ _              -> Nothing
    UserChangePassword _ password -> Just user { userPassword = password }
    PostCreation _ _ _            -> Just user { userPostCount = userPostCount user + 1 }

instance Resource Event Post where
  firstEvent event = case event of
    PostCreation pid email title -> Just $ Post pid email title
    _                            -> Nothing
  applyEvent _ _ = Nothing

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
      writeEvent events $ UserCreation (Email email) pwd
      putStrLn "User created."
    "2" -> do
      putStr "Email: "
      email <- getLine
      putStr "Password: "
      pwd <- getLine
      writeEvent events $ UserChangePassword (Email email) pwd
      putStrLn "Password changed."
    "3" -> do
      putStr "Id (number please): "
      postIdStr <- getLine
      putStr "Author e-mail: "
      email <- getLine
      putStr "Title: "
      title <- getLine
      writeEvent events $ PostCreation (PostId . read $ postIdStr) (Email email) title
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
      mres <- readResource events (Email email) :: IO (Maybe User)
      print mres
    "2" -> do
      putStr "Post Id (number please): "
      postIdStr <- getLine
      mres <- readResource events (PostId . read $ postIdStr) :: IO (Maybe Post)
      print mres
    _ -> putStrLn "Unrecognized sequence."
