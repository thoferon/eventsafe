{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.EventSafe.HTTP
  ( mkApp
  , AppConfig(..)
  , ResourceEndpoint(..)
  ) where

import           Control.Monad              (unless)

import           Data.Aeson                 hiding (encode, decode)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Monoid
import qualified Data.Aeson                 as J
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import           System.Directory

import           Database.EventSafe.Storage
import           Database.EventSafe.Types

import           Blaze.ByteString.Builder

import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai

import           Language.Haskell.TH

data CreationError
  = CreationErrorJson String
  deriving (Show, Eq)

instance ToJSON CreationError where
  toJSON err = case err of
    CreationErrorJson msg -> object
      [ "error"  .= ("json_error" :: String)
      , "detail" .= msg
      ]

-- | An endpoint together with information about how to build the resource.
data ResourceEndpoint = ResourceEndpoint
  { queryPath     :: String -- ^ The HTTP path to route from
  , toResourceRef :: String -- ^ The name of the function to call to get the 'ResourceRef' from the query string. This function must be of the form @BS.ByteString -> Either String ref@ where ref is the type of your resource reference.
  , referenceType :: String -- ^ The bane of the reference type
  , resourceType  :: String -- ^ The name of the resource type
  }

-- | Configuration of the application to pass to 'mkApp'.
data AppConfig = AppConfig
  { eventType   :: String             -- ^ Type of the events
  , storagePath :: FilePath           -- ^ Path where to store the events
  , resources   :: [ResourceEndpoint] -- ^ List of the endpoints to get the resources
  }

-- | Generate a WAI 'Application' together with an 'EventStorage'.
--
-- > do
-- >   let app = ... -- see mkWaiApp
-- >   storage <- stdInitStorage "/path/to/storage" :: IO (EventStorage [] YourEventType)
-- >   return (app storage)
mkApp :: AppConfig -> Q Exp
mkApp config = do
  eventStorageT   <- [t| EventStorage |]
  stdInitStorageE <- [| stdInitStorage |]

  appE' <- mkWaiApp config

  let appD          = FunD appN
                        [ Clause
                            [ VarP storageN
                            , VarP reqN
                            ]
                            (NormalB appE')
                            []
                        ]
      reqPathE      = LitE (StringL (storagePath config))
      eventTypeN    = mkName $ eventType config
      storageT      = AppT
                        (ConT (mkName "IO"))
                        (AppT
                          (AppT eventStorageT ListT)
                          (ConT eventTypeN))
      storageE'     = AppE stdInitStorageE reqPathE
      typedStorageE = SigE storageE' storageT
      returnAppE    = AppE
                        returnE
                        (AppE (VarE appN) (VarE storageN))
  return $ DoE
    [ LetS [appD]
    , BindS (VarP storageN) typedStorageE
    , NoBindS returnAppE
    ]

-- | Build the expression for the actual WAI 'Application'.
--
-- In this expression, we have access to the variables "storage" and "req".
--
-- > let router method path
-- >       | method == methodPost && path == [T.pack "create-event"]
-- >           = ... -- see mkCreateEventApp
-- >       | otherwise = show404Error
-- > in router (requestMethod req) (rawPathInfo req)
mkWaiApp :: AppConfig -> Q Exp
mkWaiApp config = do
  show404ErrorE  <- [| show404Error |]
  requestMethodE <- [| requestMethod |]
  rawPathInfoE   <- [| rawPathInfo |]
  otherwiseE     <- [| otherwise |]

  createEventGE <- mkCreateEventApp
  resourceGEs   <- mkResourceApps config

  let routerN     = mkName "router"
      methodE'    = AppE requestMethodE reqE
      pathE'      = AppE rawPathInfoE reqE
      routerCallE = AppE
                      (AppE (VarE routerN) methodE')
                      pathE'
      methodP     = VarP methodN
      pathP       = VarP pathN
      routerFunD  = FunD routerN [ Clause [methodP, pathP] routerB [] ]
      otherwiseG  = NormalG otherwiseE
      routerB     = GuardedB $ [createEventGE] ++ resourceGEs ++ [(otherwiseG, show404ErrorE)]

  return $ LetE [routerFunD] routerCallE

-- | THe guard together with the exp to route POST /create-event.
--
-- This expression has access to the variables "req", "storage", "method" and "path".
mkCreateEventApp :: Q (Guard, Exp)
mkCreateEventApp = do
  andE            <- [| and |]
  methodPostE     <- [| methodPost |]
  eqE             <- [| (==) |]
  stdCreateEventE <- [| stdCreateEvent |]
  createEventBsE  <- [| "/create-event" :: BS.ByteString |]

  let postMethodTest      = AppE (AppE eqE methodE) methodPostE
      createEventPathTest = AppE (AppE eqE pathE) createEventBsE
      guard               = NormalG $ AppE andE $ ListE [postMethodTest, createEventPathTest]
      expression          = AppE (AppE stdCreateEventE storageE) reqE

  return (guard, expression)

-- | Build the expressions for each resources together with the guard.
--
-- This expression has access to the variables "req", "storage", "method" and "path".
mkResourceApps :: AppConfig -> Q [(Guard, Exp)]
mkResourceApps = mapM mkResourceApp . resources

-- | See 'mkResourceApps'.
mkResourceApp :: ResourceEndpoint -> Q (Guard, Exp)
mkResourceApp endpoint = do
  andE        <- [| and |]
  eqE         <- [| (==) |]
  methodGetE  <- [| methodGet |]
  byteStringT <- [t| BS.ByteString |]

  resourceAppE <- mkResourceAppExp endpoint

  let endpointE     = SigE (LitE (StringL (queryPath endpoint))) byteStringT
      getMethodTest = AppE (AppE eqE methodE) methodGetE
      endpointTest  = AppE (AppE eqE pathE)   endpointE
      guard         = NormalG $ AppE andE $ ListE [getMethodTest, endpointTest]
      expression    = resourceAppE

  return (guard, expression)

-- | Build the expression to handle requests for resources.
--
-- This expression has access to the variables "req", "storage", "method" and "path".
--
-- > do
-- >   let mRefParam = (>>= id) . lookup "ref" . queryString $ req
-- >   case mRefParam of
-- >     Nothing       -> show400Error "{\"error\":\"no_ref_passed\",\"detail\":\"No GET parameter 'ref' has been passed.\"}"
-- >     Just refParam -> do
-- >       let eRef = yourToResourceRef refParam
-- >       case eRef :: Either String YourResourceRef of
-- >         Left err  -> show400Error $ "{\"error\":\"ref_error\",\"detail\":\"" ++ BSL.pack err ++ "\"}"
-- >         Right ref ->  do
-- >           mRes <- getResourceM storage ref
-- >           case mRes :: Maybe YourResource of
-- >             Nothing  -> show404Error
-- >             Just res -> return . ResponseBuilder ok200 [] . fromLazyByteString. J.encode $ res
mkResourceAppExp :: ResourceEndpoint -> Q Exp
mkResourceAppExp endpoint = do
  getResourceME         <- [| getResourceM |]
  getRefParamE          <- [| (>>= id) . lookup "ref" . queryString |]
  maybeT                <- [t| Maybe |]
  nothingP              <- [p| Nothing |]
  show400ErrorNoRefE    <- [| show400Error "{\"error\":\"no_ref_passed\",\"detail\":\"No GET paramater 'ref' has been passed.\"}" |]
  show404ErrorE         <- [| show404Error |]
  returnEncodedFunE     <- [| return . ResponseBuilder ok200 [] . fromLazyByteString. J.encode |]
  stringT               <- [t| String |]
  show400ErrorRefErrorE <- [| show400Error . (\errToBePacked -> "{\"error\":\"ref_error\",\"detail\":\"" <> BSL.pack errToBePacked <> "\"}") |] -- FIXME : Remove this horror

  let mRefParamN          = mkName "mRefParam"
      refParamN           = mkName "refParam"
      eRefN               = mkName "eRef"
      refN                = mkName "ref"
      errN                = mkName "err"
      mResN               = mkName "mRes"
      resN                = mkName "res"

      firstCaseStmtE      = CaseE (VarE mRefParamN)
                              [ Match nothingP (NormalB show400ErrorNoRefE) []
                              , Match (ConP (mkName "Just") [VarP refParamN]) (NormalB secondDoE) []
                              ]

      toResourceRefE      = VarE $ mkName (toResourceRef endpoint)
      secondDoE           = DoE [ LetS [ValD (VarP eRefN) (NormalB $ AppE toResourceRefE (VarE refParamN)) []]
                                , NoBindS secondCaseStmtE
                                ]
      eRefT               = AppT (AppT (ConT (mkName "Either")) stringT) (ConT (mkName (referenceType endpoint)))
      secondCaseStmtE     = CaseE (SigE (VarE eRefN) eRefT)
                              [ Match (ConP (mkName "Left")  [VarP errN]) (NormalB (AppE show400ErrorRefErrorE (VarE errN))) []
                              , Match (ConP (mkName "Right") [VarP refN]) (NormalB thirdDoE) []
                              ]

      resourceE           = AppE (AppE getResourceME storageE) (VarE refN)
      thirdDoE            = DoE [ BindS (VarP mResN) resourceE
                                , NoBindS thirdCaseStmtE
                                ]
      mResT               = AppT maybeT . ConT . mkName $ resourceType endpoint
      thirdCaseStmtE      = CaseE (SigE (VarE mResN) mResT)
                              [ Match nothingP (NormalB show404ErrorE) []
                              , Match (ConP (mkName "Just") [VarP resN]) (NormalB (AppE returnEncodedFunE (VarE resN))) []
                              ]

  return $ DoE
    [ LetS [ValD (VarP mRefParamN) (NormalB $ AppE getRefParamE reqE) []]
    , NoBindS firstCaseStmtE
    ]

-- | A 404 'Response' ready for WAI.
show404Error :: ResourceT IO Response
show404Error = return $ ResponseBuilder notFound404 [] $ fromByteString ""

-- | A 400 'Response' ready for WAI wih a customisable body.
show400Error :: BSL.ByteString -> ResourceT IO Response
show400Error = return . ResponseBuilder badRequest400 [] . fromLazyByteString

-- | A WAI 'Application' for handling POST /create-event.
stdCreateEvent :: (FromJSON e, ToJSON e, StorableEvent e, EventPool l e)
               => EventStorage l e
               -> Application
stdCreateEvent storage req = do
  result <- requestBody req $$ sinkParser (fmap fromJSON json)
  case result of
    Error   err   -> show400Error . J.encode $ CreationErrorJson err
    Success event -> do
      _ <- addEventM storage event -- We can safely ignore it as it isn't changed
      return $ ResponseBuilder created201 [] $ fromByteString ""

-- | Create a tree if missing, create an event storage configured with that path
-- and load the content inside the 'EventStorage'.
stdInitStorage :: (StorableEvent e, EventPool l e, Monoid (l e))
               => FilePath -> IO (EventStorage l e)
stdInitStorage path = do
  checkPresence <- doesDirectoryExist path
  unless checkPresence $ createDirectoryIfMissing True path
  storage <- newEventStorage path
  loadStorage storage
  return storage

storageN :: Name
storageN = mkName "storage"

storageE :: Exp
storageE = VarE storageN

appN :: Name
appN = mkName "app"

methodN :: Name
methodN = mkName "method"

methodE :: Exp
methodE = VarE methodN

pathN :: Name
pathN = mkName "path"

pathE :: Exp
pathE = VarE pathN

reqN :: Name
reqN = mkName "req"

reqE :: Exp
reqE = VarE reqN

returnN :: Name
returnN = mkName "return"

returnE :: Exp
returnE = VarE returnN
