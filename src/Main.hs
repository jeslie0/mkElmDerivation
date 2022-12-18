{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant section" #-}

module Main where
import Control.Concurrent ( newMVar, takeMVar, MVar )
import Control.Concurrent.Pool
    ( newPoolIO, noMoreTasksIO, queue, waitForIO )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Reader
    ( ReaderT(runReaderT), MonadReader(ask) )
import Crypto.Hash.SHA256 ( hashlazy )
import Data.Aeson ( decode )
import Data.Aeson.Key ( toText )
import Data.Aeson.KeyMap ( toList, KeyMap )
import Data.Bifunctor ( Bifunctor(first) )
import Data.ByteString.Builder ( toLazyByteString, byteStringHex )
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, getResponseStatus, httpLBS )
import Network.HTTP.Types.Status ( statusIsSuccessful )
import System.IO
    ( Handle,
      IOMode(WriteMode),
      hClose,
      hFlush,
      hIsOpen,
      hIsWritable,
      openFile )



-- * Types

type Version = T.Text
type Versions = [Version]

data ElmPackage = ElmPackage
  { packageName :: T.Text,
    versions :: Version
  } deriving (Eq, Show)

type ElmPackages = [ElmPackage]

data FullElmPackage = FullElmPackage
  { package :: T.Text,
    version :: T.Text,
    hash :: B.ByteString
  } deriving (Eq, Show)


-- * Data
output :: FilePath
output = "/home/james/elmNix2.nix"

src :: IO (Maybe (KeyMap Versions))
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
                mvar <- newMVar []
                B.hPut handle "{\n"
                runReaderT (downloadElmPackages mvar $ keyMapToList keyMap) handle
                B.hPut handle "}\n"

                failedPackages <- takeMVar mvar
                saveFailures failedPackages
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
keyMapToList :: KeyMap Versions -> ElmPackages
keyMapToList keymap =
  uncurry ElmPackage . first (T.fromStrict . toText) <$>
  ((\(k, vs) -> (k,) <$> vs) =<<) (toList keymap)

makeLink :: T.Text -> Version -> T.Text
makeLink name ver =
  "https://github.com/" <> name <> "/archive/refs/tags/" <> ver <> ".tar.gz"

-- * Hashing Functions

-- | Hash the given bytes and print it nicely.
prettyHash :: B.ByteString -> B.ByteString
prettyHash = toLazyByteString . byteStringHex . hashlazy

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

hashElmPackage :: ElmPackage -> ReaderT Handle IO (Maybe B.ByteString)
hashElmPackage elmPkg@(ElmPackage name ver) = do
  handle <- ask
  mBytes <- downloadPackage elmPkg
  case mBytes of
    Nothing -> return Nothing
    Just bytes -> do
      liftIO . print $ "Hashing: " <> name <> ": v" <> ver
      return . Just $ prettyHash bytes

downloadElmPackage :: MVar ElmPackages -> ElmPackage -> ReaderT Handle IO ()
downloadElmPackage mvar elmPkg@(ElmPackage name version) = do
  handle <- ask
  mHash <- hashElmPackage elmPkg
  case mHash of
    Nothing -> do
        return ()
    Just hash -> do
         liftIO . B.hPutStr handle $ toNix $ FullElmPackage name version hash
         liftIO . hFlush $ handle


downloadElmPackages :: MVar ElmPackages -> ElmPackages -> ReaderT Handle IO ()
downloadElmPackages mvar pkgs = do
  handle <- ask
  pool <- liftIO $ newPoolIO 100 False
  let elmPkgProcess rDld = runReaderT (downloadElmPackage mvar rDld) handle
      downloadPackagesInPool = mapM_ ((\io -> queue pool io ()). elmPkgProcess) $ take 100 pkgs
  liftIO downloadPackagesInPool
  liftIO $ noMoreTasksIO pool
  liftIO $ waitForIO pool

-- * To Nix function
toNix :: FullElmPackage -> B.ByteString
toNix (FullElmPackage name ver hash) =
  let nameString = encodeUtf8 name
      verString = encodeUtf8 ver
   in "    \"" <> nameString <> "\".\"" <> verString <> "\" = \"" <> hash <> "\";\n"



