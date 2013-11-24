{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.EventSafe.TypesBenchs
  ( typesBenchs
  ) where

import Control.DeepSeq

import Database.EventSafe.Types

import Criterion.Main

data Event
  = Signup String String String -- Email username password
  | Disabled String             -- Email
  | PostCreation String String  -- Email title
  deriving (Ord, Show, Eq)

data User = User String String String Int

instance NFData User where
  rnf (User email username password count) = email `seq` username `seq` password `seq` count `seq` ()

instance ResourceRef Event String where
  event `concerns` email = case event of
    Signup email' _ _     -> email == email'
    Disabled email'       -> email == email'
    PostCreation email' _ -> email == email'

instance Resource Event User where
  firstEvent event = case event of
    Signup email username password -> Just $ User email username password 0
    _                              -> Nothing

  applyEvent event user@(User email username password count) = case event of
    PostCreation _ _ -> Just $ User email username password $ count + 1
    Disabled _       -> Nothing
    _                -> Just user

events :: [Event]
events =
  [ Signup "j.smith@mail.tld" "jsmith" "pass123"
  , PostCreation "j.smith@mail.tld" "Post 1"
  , PostCreation "j.smith@mail.tld" "Post 2"
  , PostCreation "j.smith@mail.tld" "Post 3"
  , PostCreation "j.smith@mail.tld" "Post 4"
  , Signup "t.smith@mail.tld" "thomas.smith" "othersecret"
  , PostCreation "j.smith@mail.tld" "Post 5"
  , PostCreation "j.smith@mail.tld" "Post 7"
  , PostCreation "t.smith@mail.tld" "Post 1"
  , PostCreation "t.smith@mail.tld" "Post 2"
  , PostCreation "j.smith@mail.tld" "Post 6"
  , PostCreation "t.smith@mail.tld" "Post 3"
  , PostCreation "t.smith@mail.tld" "Post 4"
  , Disabled "t.smith@mail.tld"
  , PostCreation "j.smith@mail.tld" "Post 8"
  , PostCreation "t.smith@mail.tld" "Post 5"
  , PostCreation "t.smith@mail.tld" "Post 6"
  , PostCreation "j.smith@mail.tld" "Post 9"
  , PostCreation "t.smith@mail.tld" "Post 7"
  ]

typesBenchs :: Benchmark
typesBenchs = bgroup "Database.EventSafe.Types"
  [ bgroup "getResource on []"
      [ bench "actual user"  $ nf (getResource events :: String -> Maybe User) "j.smith@mail.tld"
      , bench "missing user" $ nf (getResource events :: String -> Maybe User) "un.known@mail.tld"
      ]
  ]
