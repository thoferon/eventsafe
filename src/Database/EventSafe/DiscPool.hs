{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.EventSafe.DiscPool
  ( DiscPool
  , makeDiscPool
  ) where

import           Data.Bits
import           Data.Char
import           Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Set                   as S

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans

import           System.Directory
import           System.FilePath
import           System.IO

import           Database.EventSafe.Types

-- | An 'EventPool' stored on disc.
--
-- The events are stored grouped together in \"group files\".
-- Those files are merged together when their number reaches a certain amount.
--
-- Each group file contains a list of events from oldest to newest.
-- An @index@ file is present in the directory containing a list of group files sorted as well.
data DiscPool e = DiscPool
  FilePath -- ^ Directory containing all the data.
  Int      -- ^ Half of the maximum of group files.

makePathAbsolute :: FilePath -> IO FilePath
makePathAbsolute path =
  if isAbsolute path
    then return path
    else do
      currentDir <- getCurrentDirectory
      return $ currentDir </> path

-- | Helper function to create a 'DiscPool'.
-- It will create directories and the index file if they don't exist.
makeDiscPool :: FilePath -- ^ Directory where to store everything
             -> Int      -- ^ n, so that 2*n is the maximum number of group files
             -> IO (DiscPool e)
makeDiscPool dir n = do
  dir' <- makePathAbsolute dir
  let pool = DiscPool dir' n
  createDirectoryIfMissing True $ groupsDir pool

  checkIndexPresence <- doesFileExist $ indexPath pool
  when (not checkIndexPresence) $ do
    withFile (indexPath pool) AppendMode $ \f -> hPrint f ([] :: Index)

  checkPreviousIndexPresence <- doesFileExist $ previousIndexPath pool
  when (not checkPreviousIndexPresence) $ do
    withFile (previousIndexPath pool) AppendMode $ \f -> hPrint f ([] :: Index)

  return pool

instance (MonadIO m, StorableEvent e) => EventPoolM m DiscPool e where
  emptyPoolM = return $ DiscPool "/var/eventsafe/data" 100

  filterEventsM pool ref = do
    files      <- liftIO $ getFiles pool
    eventLists <- forM files $ \path -> do
      events <- liftIO $ readEvents path
      return $ filterEvents events ref
    return $ concat eventLists

  addEventM pool event = do
    liftIO $ do
      writeEvent pool event
      tidyUpPool pool
    return pool

data IndexItem = IndexItem FilePath Int deriving (Show, Eq, Read)

instance NFData IndexItem where
  rnf (IndexItem path n) = path `seq` n `seq` ()

type Index = [IndexItem]

planMerges :: Int -> [IndexItem] -> [[IndexItem]]
planMerges goal = planMerges' goal . map (\x -> [x])

planMerges' :: Int -> [[IndexItem]] -> [[IndexItem]]
planMerges' goal items
    | length items <= goal = items
    | otherwise = case selectLowestMax totalSize $ allMerges items of
                    Nothing   -> items
                    Just best -> planMerges' goal best
  where
    selectLowestMax :: (a -> Int) -> [[a]] -> Maybe [a]
    selectLowestMax f (hd:tl) = Just . fst $ foldl (step f) (hd,maxWith f hd) tl
    selectLowestMax _ [] = Nothing

    step :: (a -> Int) -> ([a], Int) -> [a] -> ([a], Int)
    step f acc@(_,m) lst = let m' = maxWith f lst
                           in if m' < m then (lst, m') else acc

    maxWith :: (a -> Int) -> [a] -> Int
    maxWith f = maximum . map f

    totalSize :: [IndexItem] -> Int
    totalSize = foldl (\acc (IndexItem _ size) -> acc + size) 0

allMerges :: [[a]] -> [[[a]]]
allMerges lst = case lst of
  a : tl@(b : rest) ->
    let firstMerge = (a ++ b) : rest
    in firstMerge : map (a :) (allMerges tl)
  _ -> []

getFiles :: DiscPool e -> IO [FilePath]
getFiles pool = do
  index <- getIndex pool
  return $ map (\(IndexItem path _) -> path) index

-- | Return the index and block if busy.
getIndex :: DiscPool e -> IO Index
getIndex pool = do
  mres <- lockIndices pool $ \h _ -> do
            c <- hGetContents h
            return $! force (read c)
  case mres of
    Nothing  -> getIndex pool
    Just res -> return res

readEvents :: StorableEvent e => FilePath -> IO [e]
readEvents path = readEvents' `liftM` BSL.readFile path

readEvents' :: StorableEvent e => BSL.ByteString -> [e]
readEvents' "" = []
readEvents' bs =
    let (sizeBs, rest)           = BSL.splitAt 4 bs
        size                     = bsToInt sizeBs
        (serialisedEvent, rest') = BSL.splitAt size rest
    in case decode serialisedEvent of
         Just event -> event : readEvents' rest'
         Nothing    -> readEvents' rest'
  where
    bsToInt sizeBs = let chars        = BSL.unpack sizeBs
                         ~(a:b:c:d:_) = map ord chars
                     in fromIntegral $ d * 0x1000000 + c * 0x10000 + b * 0x100 + a

groupsDir :: DiscPool e -> FilePath
groupsDir (DiscPool dir _) = dir </> "groups"

indexPath :: DiscPool e -> FilePath
indexPath (DiscPool dir _) = dir </> "index"

previousIndexPath :: DiscPool e -> FilePath
previousIndexPath = flip addExtension "prev" . indexPath

writeEvent :: StorableEvent e => DiscPool e -> e -> IO ()
writeEvent pool event = do
  let encodedEvent = eventToGroup event
      md5Hash      = show $ md5 encodedEvent
      path         = groupsDir pool </> md5Hash
  changeIndex True pool $ \index -> do
    BSL.writeFile path encodedEvent
    return $ index ++ [IndexItem path 1]

changeIndex :: Bool -- ^ True: Blocking, False: Return without executing the callback if busy
            -> DiscPool e
            -> (Index -> IO Index)
            -> IO ()
changeIndex mode pool f = do
  _ <- case mode of
    False -> changeIndex' pool f
    True  -> iterateUntil id $ changeIndex' pool f
  return ()

-- | Non-blocking version of 'changeIndex'. Return whether the callback has been executed or not.
changeIndex' :: DiscPool e -> (Index -> IO Index) -> IO Bool
changeIndex' pool f = do
  mres <- lockIndices pool $ \h h2 -> do
            size     <- hFileSize h
            contents <- BSL.unpack `liftM` BSL.hGet h (fromInteger size)
            let index = read contents
            index'   <- f index
            hSeek h AbsoluteSeek 0
            hPrint h index'
            hSetFileSize h $ toInteger $ length (show index')
            hPrint h2 index
            hSetFileSize h2 $ toInteger $ length (show index)
  case mres of
    Nothing -> return False
    Just () -> return True

lockIndices :: DiscPool e -> (Handle -> Handle -> IO a) -> IO (Maybe a)
lockIndices pool f = handle (\(_ :: IOError) -> return Nothing) $ do
  h <- openFile (indexPath pool) ReadWriteMode
  handle (\(_ :: IOError) -> hClose h >> return Nothing) $ do
    h2 <- openFile (previousIndexPath pool) ReadWriteMode
    res <- f h h2
    hClose h2
    hClose h
    return $ Just res

tidyUpPool :: DiscPool e -> IO ()
tidyUpPool pool = mergeGroupFiles pool >> removeOldFiles pool

mergeGroupFiles :: DiscPool e -> IO ()
mergeGroupFiles pool@(DiscPool _ miMax) = do
  ind <- getIndex pool
  let indexSize = length ind
  when (indexSize >= 2 * miMax) $ do
    changeIndex False pool $ \index -> do
      let plan = planMerges miMax index
      forM plan $ \xs ->
        if length xs >= 2
        then writeNewGroupFile pool xs
        else return $ head xs -- will always be a singleton

writeNewGroupFile :: DiscPool e -> [IndexItem] -> IO IndexItem
writeNewGroupFile pool groupFiles = do
    contents <- BSL.concat `liftM` mapM (\(IndexItem p _) -> BSL.readFile p) groupFiles
    let md5Hash   = show $ md5 contents
        groupPath = groupsDir pool </> md5Hash
    BSL.writeFile groupPath contents
    return $ IndexItem groupPath $ totalSize groupFiles
  where totalSize = sum . map (\(IndexItem _ s) -> s)

eventToGroup :: StorableEvent e => e -> BSL.ByteString
eventToGroup event =
    let encodedEvent = encode event
        header       = intToBs $ BSL.length encodedEvent
    in BSL.concat [header, encodedEvent]
  where
    intToBs n = let a = chr $ fromIntegral n
                    b = chr $ fromIntegral (n `shift` (-8))
                    c = chr $ fromIntegral (n `shift` (-16))
                    d = chr $ fromIntegral (n `shift` (-24))
                in BSL.pack [a,b,c,d]

-- | Remove group files which are neiher in index nor in index.prev.
removeOldFiles :: DiscPool e -> IO ()
removeOldFiles pool = do
    _ <- lockIndices pool $ \h h2 -> do
           c <- hGetContents h
           c2 <- hGetContents h2
           let (ind, indp)  = (read c, read c2)
               currentPaths = S.fromList $ map (\(IndexItem path _) -> path) $ ind ++ indp
           names <- getDirectoryContents (groupsDir pool)
           let paths = map (groupsDir pool </>) names
               obsoletePaths = filter (\p -> notHidden p && p `S.notMember` currentPaths) paths
           forM_ obsoletePaths $ handle (\(_ :: IOError) -> return ()) . removeFile
    return ()
  where
    notHidden :: FilePath -> Bool
    notHidden = (/= '.') . head
