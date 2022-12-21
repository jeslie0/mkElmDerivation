{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant section" #-}

module Main where
import GHC.Generics
import qualified Data.Map as M

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
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Text.Lazy.Encoding
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.IO




-- * Types

type Name = T.Text
type Hash = T.Text
type Version = T.Text
type Versions = [ Version ]

data ElmPackage = ElmPackage
  { packageName :: T.Text,
    versi :: Version
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type ElmPackages = [ElmPackage]

data FullElmPackage = FullElmPackage
  { package :: T.Text,
    version :: T.Text,
    hash :: T.Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype ElmPackageVersionList =
  ElmPackageVersionList [ T.Text ]
                         deriving (Generic, Show, ToJSON, FromJSON)


-- * Data


base :: FilePath
base = "/home/james/"

output :: FilePath
output = base <> "elmNix.json"

failures :: FilePath
failures = base <> "elmNixFailure.json"

-- | Fetch elmPackages JSON and parse into a map.
src :: IO (Maybe (M.Map Name Versions))
src = do
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

-- * Main

main :: IO ()
main = do
  handle <- openFile output WriteMode
  fileOkay <- (&&) <$> hIsOpen handle <*> hIsWritable handle
  case fileOkay of
        False ->  print "Can't open File"
        True -> do
            mSrc <- src
            case mSrc of
              Nothing -> do
                print "Failed to parse JSON."
                hClose handle
              (Just keyMap) -> do
                sucMvar <- newMVar M.empty
                failMvar <- newMVar []
                runReaderT (downloadElmPackages sucMvar failMvar $ keyMapToList keyMap) handle

                failedPackages <- takeMVar failMvar
                saveFailures failedPackages

                successfulPackages <- takeMVar sucMvar
                B.hPut handle $ encode successfulPackages
                print "Done."
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
  uncurry ElmPackage <$>
  ((\(k, vs) -> (k,) <$> vs) =<<) (M.toList keymap)

makeLink :: T.Text -> Version -> T.Text
makeLink name ver =
  "https://github.com/" <> name <> "/archive/refs/tags/" <> ver <> ".tar.gz"

-- * Hashing Functions

-- | Hash the given bytes and print it nicely.
prettyHash :: B.ByteString -> Hash
prettyHash = decodeUtf8 . toLazyByteString . byteStringHex . hashlazy

-- * Download data

-- | Download the specified elm package into a bytestring.
downloadPackage :: ElmPackage -> ReaderT Handle IO (Maybe B.ByteString)
downloadPackage (ElmPackage name ver) = do
  handle <- ask
  liftIO . print $ "Downloading " <> name <> ": v" <> ver <> "."
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

hashElmPackage :: ElmPackage -> ReaderT Handle IO (Maybe Hash)
hashElmPackage elmPkg@(ElmPackage name ver) = do
  handle <- ask
  mBytes <- downloadPackage elmPkg
  case mBytes of
    Nothing -> return Nothing
    Just bytes -> do
      liftIO . print $ "Hashing: " <> name <> ": v" <> ver
      return . Just $ prettyHash bytes

downloadElmPackage :: MVar (M.Map Name (M.Map Version Hash)) -- ^ Hashed packages
  -> MVar ElmPackages -- ^ Failed packages
  -> ElmPackage -- ^ Package we are hashing
  -> ReaderT Handle IO ()
downloadElmPackage sucMvar failMvar elmPkg@(ElmPackage name version) = do
  handle <- ask
  mHash <- hashElmPackage elmPkg
  case mHash of
    Nothing -> do
      -- Add to failed pkgs mvar
      liftIO $ modifyMVar_ failMvar (\ps -> return (elmPkg:ps))
    Just hash -> do
      -- Add to successful pkgs mvar
      seq hash $ return ()
      liftIO $ modifyMVar_ sucMvar (return . addHashToMap elmPkg hash)



addHashToMap :: ElmPackage -- ^ Elm Package
  -> Hash -- ^ Corresponding Hash
  -> M.Map Name (M.Map Version Hash) -- ^ Map to update
  -> M.Map Name (M.Map Version Hash)
addHashToMap elmPkg@(ElmPackage name ver) hash =
  M.insertWith (\oldVerMap newVerMap -> M.insert ver hash newVerMap) name (M.insert ver hash M.empty)


downloadElmPackages :: MVar (M.Map Name (M.Map Version Hash)) -- ^
  -> MVar ElmPackages -- ^
  -> ElmPackages -- ^
  -> ReaderT Handle IO ()
downloadElmPackages sucMvar failMvar pkgs = do
  handle <- ask
  pool <- liftIO $ newPoolIO 100 False
  let elmPkgProcess rDld = runReaderT (downloadElmPackage sucMvar failMvar rDld) handle
      downloadPackagesInPool = mapM_ ((\io -> queue pool io ()). elmPkgProcess) $ take 100 pkgs
  liftIO downloadPackagesInPool
  liftIO $ noMoreTasksIO pool
  liftIO $ waitForIO pool

-- * To Nix function
-- toNix :: FullElmPackage -> B.ByteString
-- toNix (FullElmPackage name ver hash) =
--   let nameString = encodeUtf8 name
--       verString = encodeUtf8 ver
--    in "    \"" <> nameString <> "\".\"" <> verString <> "\" = \"" <> hash <> "\";\n"



-- * Json experiments


srcTest :: IO (Maybe (M.Map String ElmPackageVersionList))
srcTest = do
  print "Fetching elmPackages.json"
  bytes <- B.readFile "/home/james/Documents/Projects/elmNix/test.json"
  return . decode $ bytes

foo :: Maybe (M.Map String [String])
foo = decode $ encode mp
  where
    mp = M.insert "Hi" (ElmPackageVersionList ["Value"]) M.empty :: M.Map String ElmPackageVersionList
