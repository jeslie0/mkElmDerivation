{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant section" #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Pool
import Control.Monad.IO.Class
import Control.Monad.Reader
import Crypto.Hash.SHA256
import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Builder
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO as TI
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.IO

-- * Types

type Name = T.Text

type Hash = T.Text

type Version = T.Text

type Versions = [Version]

data ElmPackage = ElmPackage
  { packageName :: T.Text,
    version :: Version
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type ElmPackages = [ElmPackage]

data ReadState = ReadState
  { hashedPkgsMvar :: MVar (M.Map Name (M.Map Version Hash))
  , failedPkgsMvar :: MVar ElmPackages
  , successesMvar :: MVar Int
  , outputHandle :: Handle
  }


-- * Data

base :: FilePath
base = "/home/james/"

output :: FilePath
output = base <> "elmNix2.json"

failures :: FilePath
failures = base <> "elmNixFailure.json"

-- | Fetch elmPackages JSON and parse into a map.
remoteSrc :: IO (Maybe (M.Map Name Versions))
remoteSrc = do
  print "Fetching elmPackages.json"
  req <- parseRequest "https://package.elm-lang.org/all-packages"
  resp <- httpLBS req
  let status = getResponseStatus resp
  if statusIsSuccessful status
    then do
      return . decode . getResponseBody $ resp
    else do
      print "Could not fetch json file"
      return Nothing

localSrc :: IO (Maybe (M.Map Name Versions))
localSrc = do
  decode <$> B.readFile "/home/james/Documents/Projects/elmNix/package.elm-lang.org.json"

-- * Main

main :: IO ()
main = do
  handle <- openFile output ReadMode
  fileOkay <- (&&) <$> hIsOpen handle <*> hIsReadable handle
  case fileOkay of
    False -> print "Can't open File"
    True -> canOpenFile handle

canOpenFile :: Handle -> IO ()
canOpenFile handle = do
  mSrc <- remoteSrc
  case mSrc of
    Nothing -> do
      print "Failed to parse JSON."
      hClose handle
    (Just keyMap) -> parsedJson keyMap handle

parsedJson :: M.Map Name Versions -> Handle -> IO ()
parsedJson keyMap handle = do
  alreadyHashedM <- decode <$> B.hGetContents handle :: IO (Maybe (M.Map Name (M.Map Version Hash)))
  seq alreadyHashedM (return ())
  newHandle <- openFile output WriteMode
  case alreadyHashedM of
    Just alreadyHashed -> do
      let toHash = extractNewPackages keyMap alreadyHashed
      seq toHash $ go alreadyHashed toHash newHandle
    Nothing -> do
      print "Failed to parse output file when checking prehashed files"
      go M.empty keyMap newHandle

go :: M.Map Name (M.Map Version Hash) -> M.Map Name Versions -> Handle -> IO ()
go alreadyHashed toHash handle = do
  sucMvar <- newMVar M.empty
  failMvar <- newMVar []
  countMvar <- newMVar 0

  runReaderT (downloadElmPackages $ keyMapToList toHash) $ ReadState sucMvar failMvar countMvar handle

  failedPackages <- takeMVar failMvar
  saveFailures failedPackages

  successfulPackages <- takeMVar sucMvar
  B.hPut handle . encode $ joinNewPackages successfulPackages alreadyHashed

  count <- takeMVar countMvar
  print $ "Done. Downloaded and hashed " <> show count <> " packages"
  hClose handle


saveFailures :: ElmPackages -> IO ()
saveFailures [] = print "No failures to save."
saveFailures pkgs = do
  handle <- openFile "/home/james/Documents/Projects/elmNix/failures" WriteMode
  fileOkay <- (&&) <$> hIsOpen handle <*> hIsWritable handle
  case fileOkay of
    False -> print "Can't open failures file."
    True -> do
      mapM_ (TI.hPutStrLn handle . (\(ElmPackage nom ve) -> nom <> ": " <> ve)) pkgs
      hClose handle

-- * Helper Functions

keyMapToList :: M.Map Name Versions -> ElmPackages
keyMapToList keymap =
  uncurry ElmPackage
    <$> ((\(k, vs) -> (k,) <$> vs) =<<) (M.toList keymap)

makeLink :: T.Text -> Version -> T.Text
makeLink name ver =
  "https://github.com/" <> name <> "/archive/refs/tags/" <> ver <> ".tar.gz"

-- * Hashing Functions

-- | Hash the given bytes and print it nicely.
prettyHash :: B.ByteString -> Hash
prettyHash = decodeUtf8 . toLazyByteString . byteStringHex . hashlazy


-- * Download data

-- | Download the specified elm package into a bytestring.
downloadPackage :: ElmPackage -> IO (Maybe B.ByteString)
downloadPackage (ElmPackage name ver) = do
  print $ "Downloading " <> name <> ": v" <> ver <> "."
  req <- parseRequest . T.unpack $ makeLink name ver
  resp <- httpLBS req
  let status = getResponseStatus resp
  if statusIsSuccessful status
    then do
      liftIO . print $ "Successfully downloaded " <> name <> "."
      return . Just $ getResponseBody resp
    else do
      liftIO . print $ "Failed to fetch " <> name <> " " <> ver <> "."
      return Nothing


hashElmPackage :: ElmPackage -> IO (Maybe Hash)
hashElmPackage elmPkg@(ElmPackage name ver) = do
  mBytes <- downloadPackage elmPkg
  case mBytes of
    Nothing -> return Nothing
    Just bytes -> do
      liftIO . print $ "Hashing: " <> name <> ": v" <> ver
      return . Just $ prettyHash bytes


downloadElmPackage :: ElmPackage -> ReaderT ReadState IO ()
downloadElmPackage elmPkg = do
  (ReadState sucMvar failMvar countMvar _) <- ask
  mHash <- liftIO $ hashElmPackage elmPkg
  case mHash of
    Nothing -> do
      -- Add to failed pkgs mvar
      liftIO $ modifyMVar_ failMvar (\ps -> return (elmPkg : ps))
    Just hash -> do
      -- Add to successful pkgs mvar
      seq hash $ return ()
      liftIO $ modifyMVar_ sucMvar (return . addHashToMap elmPkg hash)
      liftIO $ modifyMVar_ countMvar (\n -> return $! n + 1)

addHashToMap ::
  -- | Elm Package
  ElmPackage ->
  -- | Corresponding Hash
  Hash ->
  -- | Map to update
  M.Map Name (M.Map Version Hash) ->
  M.Map Name (M.Map Version Hash)
addHashToMap (ElmPackage name ver) hash =
  M.insertWith (\oldVerMap newVerMap -> M.insert ver hash newVerMap) name (M.insert ver hash M.empty)

downloadElmPackages :: ElmPackages -> ReaderT ReadState IO ()
downloadElmPackages pkgs = do
  state <- ask
  pool <- liftIO $ newPoolIO 100 False
  let elmPkgProcess rDld = runReaderT (downloadElmPackage rDld) state
      downloadPackagesInPool = mapM_ ((\io -> queue pool io ()) . elmPkgProcess) pkgs
  liftIO downloadPackagesInPool
  liftIO $ noMoreTasksIO pool
  liftIO $ waitForIO pool

-- * To Nix function

-- toNix :: FullElmPackage -> B.ByteString
-- toNix (FullElmPackage name ver hash) =
--   let nameString = encodeUtf8 name
--       verString = encodeUtf8 ver
--    in "    \"" <> nameString <> "\".\"" <> verString <> "\" = \"" <> hash <> "\";\n"

foo :: FilePath -> IO (Maybe (M.Map Name (M.Map Version Hash)))
foo path = decode <$> B.readFile path


extractNewPackages :: (Ord a, Ord b) => M.Map a [b] -> M.Map a (M.Map b c) -> M.Map a [b]
extractNewPackages =
  M.differenceWithKey (\key vers verHashMap -> let newVers = [ v | v <- vers, not (v `M.member` verHashMap) ]
                     in if null newVers then Nothing else Just newVers)

joinNewPackages :: (Ord a, Ord b) => M.Map a (M.Map b c) -> M.Map a (M.Map b c) -> M.Map a (M.Map b c)
joinNewPackages =
  M.unionWith M.union
