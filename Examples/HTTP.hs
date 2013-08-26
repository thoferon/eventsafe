{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.HTTP where

import Data.Maybe
import qualified Data.ByteString.Char8 as BS

import Network.Wai.Handler.Warp (run)

import Database.EventSafe

import Examples.Shared

mkEmail :: BS.ByteString -> Either String Email
mkEmail bs = case bs of
  "special@server.tld" -> Left "This email is special and returns an error."
  _                    -> Right . Email . BS.unpack $ bs

mkPostId :: BS.ByteString -> Either String PostId
mkPostId bs =
  let mNum = (>>= fst) . listToMaybe . reads . BS.unpack $ bs
  in case mNum of
      Nothing  -> Left "Id should be an integer."
      Just id' -> Right $ PostId id'

main :: IO ()
main = run 1337 =<< $(mkApp AppConfig
  { eventType   = "Event"
  , storagePath = "tmp"
  , resources   =
      [ ResourceEndpoint "/user" "mkEmail"  "Email"  "User"
      , ResourceEndpoint "/post" "mkPostId" "PostId" "Post"
      ]
  })
