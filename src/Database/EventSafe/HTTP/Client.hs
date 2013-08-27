{-# LANGUAGE OverloadedStrings #-}

module Database.EventSafe.HTTP.Client
  ( apiGetResource
  , apiCreateEvent
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Network.HTTP
import           Network.URI

-- | A helper to build a 'Request BSL.ByteString', setting the content length properly in the header
buildRequest :: RequestMethod
             -> URI
             -> BSL.ByteString -- ^ The request body
             -> Request BSL.ByteString
buildRequest method url body =
    setContentLength . setBody $ (mkRequest method url :: Request BSL.ByteString)
  where
    setBody req      = req { rqBody = body }
    setContentLength = replaceHeader HdrContentLength $ show (BSL.length body)

apiGetResource :: FromJSON res
               => (ref -> String) -- ^ Function to build the /ref/ parameter of the request
               -> String          -- ^ Endpoint, e.g. "/users"
               -> URI             -- ^ The base URL of the database, e.g. http://localhost:1337
               -> ref             -- ^ The reference to the
               -> IO (Either String res)
apiGetResource mkRefParam frag baseUrl ref = do
  let endpoint = baseUrl
        { uriPath  = uriPath baseUrl ++ frag
        , uriQuery = "?ref=" ++ mkRefParam ref
        }
      req :: Request BSL.ByteString
      req = mkRequest GET endpoint
  eresp <- simpleHTTP req
  return $ case eresp of
    Left err   -> Left $ "Error from HTTP: " ++ show err
    Right resp ->
      let mRes = decode $ rspBody resp
      in case mRes of
        Nothing  -> Left "Can't parse the JSON response"
        Just res -> Right res

apiCreateEvent :: ToJSON event => URI -> event -> IO (Either String ())
apiCreateEvent baseUrl event = do
  let endpoint = baseUrl { uriPath = uriPath baseUrl ++ "/create-event" }
      req :: Request BSL.ByteString
      req = buildRequest POST endpoint $ encode event
  eresp <- simpleHTTP req
  return $ case eresp of
    Left err   -> Left $ "Error from HTTP: " ++ show err
    Right resp -> case rspCode resp of
      (2, 0, 1) -> Right ()
      _         -> Left "Error code returned" -- FIXME (return a Maybe CreationError ?)
