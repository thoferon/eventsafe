{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.Shared where

import Database.EventSafe
import GHC.Generics
import qualified Data.Aeson as Json

newtype Email = Email String deriving (Show, Eq, Generic)
instance Json.ToJSON Email
instance Json.FromJSON Email

data User = User {
  userEmail       :: Email
  , userPassword  :: String
  , userPostCount :: Int
} deriving (Show, Eq)

newtype PostId = PostId Int deriving (Show, Eq, Generic)
instance Json.ToJSON PostId
instance Json.FromJSON PostId

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
instance Json.FromJSON Event

instance StorableEvent Event where
  encode = Json.encode
  decode = Json.decode

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
