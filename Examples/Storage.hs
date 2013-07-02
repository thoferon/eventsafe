{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.Storage where

import Database.EventSafe
import Control.Concurrent.STM
import System.IO
import System.Directory
import System.IO.Temp
import GHC.Generics
import Data.Time
import qualified Data.Aeson as Json
import Control.Applicative

newtype Email = Email String deriving (Show, Eq, Generic)
instance Json.ToJSON Email

data User = User {
  userEmail       :: Email
  , userPassword  :: String
  , userPostCount :: Int
} deriving (Show, Eq)

newtype PostId = PostId Int deriving (Show, Eq, Generic)
instance Json.ToJSON PostId

data Post = Post {
  postId            :: PostId
  , postAuthorEmail :: Email
  , postTitle       :: String
} deriving (Show, Eq)

data Event
  = UserCreation String Email String
  | UserChangePassword String Email String
  | PostCreation String PostId Email String
  deriving (Show, Eq, Generic)

instance Json.ToJSON Event

instance StorableEvent Event where
  encode = Json.encode

instance Ord Event where
  e1 `compare` e2 = getTime e1 `compare` getTime e2
    where getTime event = case event of
            UserCreation time _ _       -> time
            UserChangePassword time _ _ -> time
            PostCreation time _ _ _     -> time

instance ResourceRef Event Email where
  event `concerns` email = case event of
    UserCreation       _ email' _ -> email == email'
    UserChangePassword _ email' _ -> email == email'
    PostCreation     _ _ email' _ -> email == email'

instance ResourceRef Event PostId where
  event `concerns` pid = case event of
    PostCreation _ pid' _ _ -> pid == pid'
    _                       -> False

instance Resource Event User where
  firstEvent event = case event of
    UserCreation _ email password -> Just $ User email password 0
    _                             -> Nothing

  applyEvent event user = case event of
    UserCreation _ _ _              -> Nothing
    UserChangePassword _ _ password -> Just user { userPassword = password }
    PostCreation _ _ _ _            -> Just user { userPostCount = userPostCount user + 1 }

instance Resource Event Post where
  firstEvent event = case event of
    PostCreation _ pid email title -> Just $ Post pid email title
    _                              -> Nothing
  applyEvent _ _ = Nothing

-- Naive simple program prompting the user allowing him/her
-- to add new events and get resources
-- (horrible piece of code but it's just an example)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  tvar <- newTVarIO []
  withSystemTempDirectory "eventsafe-examples-storage" $ \dir -> do
    createDirectoryIfMissing True dir
    putStrLn $ "Storage in " ++ dir
    let storage = EventStorage tvar dir
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
      writeEvent storage $ UserCreation time (Email email) pwd
      putStrLn "User created."
    "2" -> do
      putStr "Email: "
      email <- getLine
      putStr "Password: "
      pwd <- getLine
      time <- show <$> getCurrentTime
      writeEvent storage $ UserChangePassword time (Email email) pwd
      putStrLn "Password changed."
    "3" -> do
      putStr "Id (number please): "
      postIdStr <- getLine
      putStr "Author e-mail: "
      email <- getLine
      putStr "Title: "
      title <- getLine
      time <- show <$> getCurrentTime
      writeEvent storage $ PostCreation time (PostId . read $ postIdStr) (Email email) title
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
      mres <- readResource events (Email email) :: IO (Maybe User)
      print mres
    "2" -> do
      putStr "Post Id (number please): "
      postIdStr <- getLine
      mres <- readResource events (PostId . read $ postIdStr) :: IO (Maybe Post)
      print mres
    _ -> putStrLn "Unrecognized sequence."
