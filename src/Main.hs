{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Main where

import Control.Monad (join)
import Control.Monad.Reader
    ( MonadIO(liftIO), MonadReader(ask), ReaderT(runReaderT) )
import Control.Monad.State ( MonadIO(liftIO) )
import Control.Monad.Writer
    ( MonadIO(liftIO), MonadWriter(tell), WriterT(runWriterT) )
import Crypto.Hash.SHA256 (hashlazy)
import Data.Aeson (decode, encode)
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (KeyMap, toList)
import Data.Bifunctor (first)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics ()
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatus,
    httpLBS,
    parseRequest,
  )
import Network.HTTP.Types.Status (statusIsSuccessful)
import System.IO
  ( Handle,
    IOMode (WriteMode),
    hClose,
    hFlush,
    hIsOpen,
    hIsWritable,
    openFile
  )
import System.TimeIt ( timeIt )



-- * Types

type Version = T.Text
type Versions = [Version]
data ElmPackage = ElmPackage
  { packageName :: T.Text,
    versions :: Versions
  } deriving (Eq, Show)

type ElmPackages = [ElmPackage]

data FullElmPackage = FullElmPackage
  { package :: T.Text,
    version :: T.Text,
    hash :: B.ByteString
  } deriving (Eq, Show)

data Package =
  Package { name :: T.Text
          , ver :: T.Text
          } deriving (Show)

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
main = timeIt $ do
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
                let result = runWriterT (downloadElmPackages $ keyMapToList keyMap)
                B.hPut handle "{\n"
                (_, failures) <- runReaderT result handle
                saveFailures failures
                B.hPut handle "}\n"
                print "Done."
                hClose handle

saveFailures :: [Package] -> IO ()
saveFailures [] = print "No failures to save."
saveFailures pkgs = do
  handle <- openFile "/home/james/Documents/Projects/elmNix/failures" WriteMode
  fileOkay <- (&&) <$> hIsOpen handle <*> hIsWritable handle
  case fileOkay of
    False -> print "Can't open failures file."
    True -> do
      mapM_ (TI.hPutStrLn handle . (\(Package nom ve) -> nom <> ": " <> ve)) pkgs
      hClose handle



-- * Helper Functions
keyMapToList :: KeyMap Versions -> ElmPackages
keyMapToList map =
  uncurry ElmPackage . first (T.fromStrict . toText) <$> toList map


makeLink :: T.Text -> Version -> T.Text
makeLink name ver =
  "https://github.com/" <> name <> "/archive/refs/tags/" <> ver <> ".tar.gz"

-- * Hashing Functions

-- | Hash the given bytes and print it nicely.
prettyHash :: B.ByteString -> B.ByteString
prettyHash = toLazyByteString . byteStringHex . hashlazy

-- * Download data

downloadPackage :: T.Text -> Version -> WriterT [Package] (ReaderT Handle IO) (Maybe B.ByteString)
downloadPackage name ver = do
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
      tell [Package name ver]
      return Nothing

hashElmPackage :: T.Text -> Version -> WriterT [Package] (ReaderT Handle IO) (Maybe B.ByteString)
hashElmPackage name ver = do
  handle <- ask
  mBytes <- downloadPackage name ver
  case mBytes of
    Nothing -> return Nothing
    Just bytes -> do
      liftIO . print $ "Hashing: " <> name <> ": v" <> ver
      return . Just $ prettyHash bytes

downloadElmPackage :: ElmPackage -> WriterT [Package] (ReaderT Handle IO) ()
downloadElmPackage (ElmPackage name versions) =
  mapM_
    ( \ver -> do
        handle <- ask
        mHash <- hashElmPackage name ver
        case mHash of
          Nothing -> do
            return ()
          (Just hash) -> do
            liftIO . B.hPutStr handle $ toNix $ FullElmPackage name ver hash
            liftIO . hFlush $ handle
    )
    versions

downloadElmPackages :: ElmPackages -> WriterT [Package] (ReaderT Handle IO) ()
downloadElmPackages = mapM_ downloadElmPackage

-- * To Nix function
toNix :: FullElmPackage -> B.ByteString
toNix (FullElmPackage name ver hash) =
  let nameString = encodeUtf8 name
      verString = encodeUtf8 ver
   in "    \"" <> nameString <> "\".\"" <> verString <> "\" = \"" <> hash <> "\";\n"
