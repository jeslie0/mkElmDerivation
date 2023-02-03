{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Pool
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Crypto.Hash as C
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Directory
import System.IO

-- * Types

type Name = T.Text

type Hash = T.Text

type Version = T.Text

type Versions = V.Vector Version

data ElmPackage = ElmPackage
  { packageName :: T.Text
  , version :: Version
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type ElmPackages = V.Vector ElmPackage

data ReadState = ReadState
  { hashedPkgsMvar :: MVar (M.Map Name (M.Map Version Hash))
  , failedPkgsMvar :: MVar (M.Map Name Versions)
  }

-- * Main

main :: IO (Maybe ())
main = runMaybeT $ do
  -- The handle for the output file containing the successfully hashed
  -- packages.
  outputHandle <- handleOutputFile
  alreadyHashedPkgs <- decodeToMap outputHandle :: MaybeT IO (M.Map Name (M.Map Version Hash))

  -- The handle for the output failures file.
  failuresHandle <- handleFailureFile
  previousFailuresNVsMap <- decodeToMap failuresHandle :: MaybeT IO (M.Map Name Versions)

  -- The keymap obtained from downloading and parsing
  -- elm-packages.json.
  elmJsonNVsMap <- remoteSrc

  pkgsToHash <- liftIO $ getNewPkgsToHash alreadyHashedPkgs previousFailuresNVsMap elmJsonNVsMap

  (newFailures, newSuccesses) <- liftIO $ getFailuresSuccesses pkgsToHash

  newOutputHandle <- handleOutputFile
  newFailuresHandle <- handleFailureFile

  liftIO $ saveSuccesses newOutputHandle newSuccesses alreadyHashedPkgs
  liftIO $ saveFailures newFailuresHandle newFailures previousFailuresNVsMap

  liftIO $ hClose newOutputHandle
  liftIO $ hClose newFailuresHandle

  liftIO $ printReport previousFailuresNVsMap alreadyHashedPkgs elmJsonNVsMap pkgsToHash newFailures newSuccesses

-- * Input and Output Files
-- This contains IO actions returning the locations of the output
-- files for reading and saving successes and failures, as well as a
-- fetcher and parser for the remote elm-packages.json.

-- | The absolute path of the output file, to read and save successes.
output :: IO FilePath
output = makeAbsolute "./elmData.json"

-- | The absolute path of the failures file, to read and save failures.
failures :: IO FilePath
failures = makeAbsolute "./failures.json"

-- | Fetch elmPackages JSON and parse into a map.
remoteSrc :: MaybeT IO (M.Map Name Versions)
remoteSrc = do
  liftIO $ print "Fetching elmPackages.json"
  req <- parseRequest "https://package.elm-lang.org/all-packages"
  resp <- httpLBS req
  let status = getResponseStatus resp
  if statusIsSuccessful status
    then MaybeT . return . decode . getResponseBody $ resp
    else do
      liftIO $ print "Could not fetch json file"
      MaybeT . return $ Nothing

-- * Download data

-- | Take a name and a version and output the link to download it
-- from.
makeLink :: Name ->  -- ^ Name of Elm package (e.g elm/elm-ui).
  Version ->  -- ^ Version of Elm package (e.g 0.1.0)
  T.Text
makeLink name ver =
  "https://github.com/" <> name <> "/archive/" <> ver <> ".tar.gz"

-- | Hash the given bytes and print it nicely.
prettyHash :: B.ByteString -> -- ^ SHA256 these bytes.
  Hash
prettyHash bytes = T.pack . show $ (C.hashlazy bytes :: C.Digest C.SHA256)

-- | Download the specified elm package into a bytestring.
downloadPackage :: ElmPackage -> -- ^ Elm package to download.
  MaybeT IO B.ByteString -- ^ Package wrapped up as bytes.
downloadPackage (ElmPackage name ver) = do
  liftIO . print $ "Downloading " <> name <> ": v" <> ver <> "."
  req <- parseRequest . T.unpack $ makeLink name ver
  resp <- httpLBS req
  let status = getResponseStatus resp
  if statusIsSuccessful status
    then do
      liftIO . print $ "Successfully downloaded " <> name <> "."
      MaybeT . return . Just $ getResponseBody resp
    else do
      liftIO . print $ "Failed to fetch " <> name <> " " <> ver <> "."
      MaybeT . return $ Nothing

-- | Download and hash the given ElmPackage.
hashElmPackage :: ElmPackage -> -- ^ The Elm package to hash.
  MaybeT IO Hash -- ^ The wrapped up hash of the given package.
hashElmPackage elmPkg@(ElmPackage name ver) = do
  bytes <- downloadPackage elmPkg
  liftIO . print $ "Hashing: " <> name <> ": v" <> ver
  let !hash = prettyHash bytes
  liftIO . print $ "Hashed: " <> name <> ": v" <> ver
  return hash

-- | Download and hash the given elm package, adding the data to the
-- MVars in the ReadState.
downloadElmPackage :: ElmPackage -> -- ^ The Elm package to operate on.
  ReaderT ReadState IO ()
downloadElmPackage elmPkg@(ElmPackage name ver) = do
  (ReadState sucMvar failMvar) <- ask
  mHash <- liftIO . runMaybeT $ hashElmPackage elmPkg
  case mHash of
    Nothing -> do
      -- Add to failed pkgs mvar
      failMap <- liftIO $ takeMVar failMvar
      liftIO $ putMVar failMvar (M.insertWith (<>) name [ver] failMap)
    Just hash -> do
      -- Add to successful pkgs mvar
      succMap <- liftIO $ takeMVar sucMvar
      liftIO $ putMVar sucMvar (addHashToMap elmPkg hash succMap)

-- | Downloads all of the elm packages asynchronously, using 100
-- threads.
downloadElmPackages :: M.Map Name Versions -> -- ^ Map of package
                       -- names and versions to hash.
  ReaderT ReadState IO ()
downloadElmPackages pkgs = do
  state <- ask
  pool <- liftIO $ newPoolIO 100 False
  let elmPkgProcess rDld = runReaderT (downloadElmPackage rDld) state
      runElmPkgProcess name vers = mapM_ ((\io -> queue pool io ()) . elmPkgProcess) $ ElmPackage name <$> vers
      downloadPackagesInPool = mapMWithKey_ runElmPkgProcess pkgs
  liftIO downloadPackagesInPool
  liftIO $ noMoreTasksIO pool
  liftIO $ waitForIO pool

-- * File and handle management

handleOutputFile :: MaybeT IO Handle
handleOutputFile = do
  outputPath <- liftIO output
  handle <- liftIO $ openFile outputPath ReadWriteMode
  fileOkay <- liftIO $ (&&) <$> hIsOpen handle <*> hIsReadable handle
  if fileOkay
    then return handle
    else do
      liftIO . print $ "Can't open output file"
      MaybeT . return $ Nothing

decodeToMap :: (FromJSONKey a, Ord a, FromJSON b) => Handle -> MaybeT IO (M.Map a b)
decodeToMap handle = do
  contents <- liftIO $ B.hGetContents handle
  case decode contents of
    Just failureMap -> return failureMap
    Nothing -> return M.empty

handleFailureFile :: MaybeT IO Handle
handleFailureFile = do
  failurePath <- liftIO failures
  handle <- liftIO $ openFile failurePath ReadWriteMode
  fileOkay <- liftIO $ (&&) <$> hIsOpen handle <*> hIsReadable handle
  if fileOkay
    then return handle
    else do
      liftIO $ print "Can't open failures file"
      MaybeT . return $ Nothing

getPreviousFailures :: Handle -> MaybeT IO (M.Map Name Versions)
getPreviousFailures handle = undefined

getNewPkgsToHash ::
  -- | Already hashed packages.
  M.Map Name (M.Map Version Hash) ->
  -- | Previous failed packages
  M.Map Name Versions ->
  -- | All elm-packages.json.
  M.Map Name Versions ->
  IO (M.Map Name Versions)
getNewPkgsToHash alreadyHashed failuresMap allPkgsMap = do
  return . removeFailedPkgs (extractNewPackages allPkgsMap alreadyHashed) $ failuresMap

getFailuresSuccesses ::
  M.Map Name Versions ->
  IO (M.Map Name Versions, M.Map Name (M.Map Version Hash))
getFailuresSuccesses toHash = do
  sucMvar <- newMVar M.empty
  failMvar <- newMVar M.empty

  runReaderT (downloadElmPackages toHash) $ ReadState sucMvar failMvar

  failedPackages <- takeMVar failMvar
  successfulPackages <- takeMVar sucMvar
  return (failedPackages, successfulPackages)

saveSuccesses ::
  Handle ->
  M.Map Name (M.Map Version Hash) ->
  M.Map Name (M.Map Version Hash) ->
  IO ()
saveSuccesses handle successfulPackages alreadyHashed = do
  B.hPut handle . encode $ joinNewPackages successfulPackages alreadyHashed

-- | Save the given ElmPackages to a file. Used to record which
-- packages failed to download.
saveFailures ::
  Handle ->
  M.Map Name Versions ->
  M.Map Name Versions ->
  IO ()
saveFailures handle failedPackages alreadyFailed = do
  B.hPut handle . encode $ updateFailures failedPackages alreadyFailed

-- * M.Map helper functions

-- | Inserts the Elm package information and hash into the given map.
addHashToMap :: ElmPackage -> -- ^ Elm Package
  Hash -> -- ^ Corresponding Hash
  M.Map Name (M.Map Version Hash) -> -- ^ Map to update
  M.Map Name (M.Map Version Hash)
addHashToMap (ElmPackage name ver) hash =
  M.insertWith (\oldVerMap newVerMap -> M.insert ver hash newVerMap) name (M.insert ver hash M.empty)


-- | Filters the keys and values in the map by terms of the second
-- that don't appear.
-- >>> extractNewPackages [(1, [1,2,3])] [(1,[(2,3)])]
-- fromList [(1,[1,3])]
extractNewPackages :: (Ord a, Ord b) => M.Map a (V.Vector b) ->
  M.Map a (M.Map b c) ->
  M.Map a (V.Vector b)
extractNewPackages =
  M.differenceWith
    ( \vers verHashMap ->
        let newVers = do
              v <- vers
              guard $ not (v `M.member` verHashMap)
              return v
         in if null newVers then Nothing else Just newVers
    )

-- | Removes terms from the first map that are also in the second
-- list.
-- >>> removeFailedPkgs [(1, [1,2,3]), (2, [3])]   [(1, [1]), (2, [3])]
-- fromList [(1,[2,3])]
removeFailedPkgs ::
  (Ord a, Eq b) =>
  M.Map a (V.Vector b) -> -- ^ Map of failures.
  M.Map a (V.Vector b) -> -- ^ Map of things to hash.
  M.Map a (V.Vector b)
removeFailedPkgs =
  M.differenceWith $
    \vers badVers ->
      if vers == badVers
        then Nothing
        else Just $ vers \\ badVers

-- | Combines the two maps with both unions and keys, favouring the
-- keys on the left.
-- >>> joinNewPackages [(1, [(2, 3), (0, 4)])] [(1, [(5, 6), (2, 9)])]
-- fromList [(1,fromList [(0,4),(2,3),(5,6)])]
joinNewPackages :: (Ord a, Ord b) =>
  M.Map a (M.Map b c) -> -- ^ First map.
  M.Map a (M.Map b c) -> -- ^ Second map.
  M.Map a (M.Map b c)
joinNewPackages =
  M.unionWith M.union

-- | Generates a new map that is the union of the given two, whose
-- values are combined.
-- >>> updateFailures [(1,[2])] [(1, [1]), (2, [3] :: [Int])]
-- fromList [(1,[2,1]),(2,[3])]
updateFailures ::
  (Ord a, Monoid b) =>
  M.Map a b -> -- ^ Map of failures
  M.Map a b -> -- ^ Map of new failures to hash
  M.Map a b
updateFailures =
  M.unionWith $
    \badVers vers -> badVers <> vers

-- | A Vector based difference function.
-- >>> V.fromList [1,2,3] \\ (V.fromList [2,3])
-- [1]
(\\) :: (Eq a) => V.Vector a -> V.Vector a -> V.Vector a
(\\) = V.foldl' (\as b -> V.filter (b /=) as)

-- | Map a monadic action (which uses both keys and values) over a
-- map, keeping the result.
mapMWithKey :: (Monad m) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
mapMWithKey f kvMap = sequence $ M.mapWithKey f kvMap

-- | Map a monadic action (which uses both keys and values) over a
-- map, for sideffects only.
mapMWithKey_ :: (Monad m) => (k -> a -> m b) -> M.Map k a -> m ()
mapMWithKey_ f kvMap = sequence_ $ M.mapWithKey f kvMap


-- | Prints a report about the past and current packages.
printReport :: M.Map Name Versions -> -- ^ Previous failures
  M.Map Name (M.Map Version Hash) -> -- ^ Previously hashed packages
  M.Map Name Versions -> -- ^ Packages in elm-packages.json
  M.Map Name Versions -> -- ^ Packages we are hashing
  M.Map Name Versions -> -- ^ New Failures
  M.Map Name (M.Map Version Hash) -- ^ New packages hashed
  -> IO ()
printReport prevFails prevHashed elmPkgs toHash newFails newSuccesses = do
  putStrLn $ "Number of previous failed packages: " ++ show (sumOfMap prevFails) ++ "."
  putStrLn $ "Number of previous hashed packages: " ++ show (sumOfMap prevHashed) ++ "."
  putStrLn $ "Number of elm packages: " ++ show (sumOfMap elmPkgs) ++ "."
  putStrLn $ "Number of new packages to hash: " ++ show (sumOfMap toHash) ++ "."
  putStrLn $ "Number of new packages which failed: " ++ show (sumOfMap newFails) ++ "."
  putStrLn $ "Number of new packages hashed: " ++ show (sumOfMap newSuccesses) ++ "."
  where
    sumOfMap :: (Foldable t) => M.Map a (t b) -> Int
    sumOfMap = M.foldl' (\a b -> a + length b) 0
