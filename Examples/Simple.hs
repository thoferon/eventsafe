{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.Simple where

import Database.EventSafe

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

events :: [Event]
events = [
  UserCreation         (Email "foo@bar.com")   "secret"
  , UserChangePassword (Email "foo@bar.com")   "new"
  , UserCreation       (Email "other@bar.com") "pwd"
  , PostCreation       (PostId 33524)          (Email "foo@bar.com") "First post"
  , UserChangePassword (Email "foo@bar.com")   "again"
  , PostCreation       (PostId 92013)          (Email "foo@bar.com") "Second post"
  ]

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

main :: IO ()
main = do
  let muser = getResource events (Email "foo@bar.com") :: Maybe User
  print muser
