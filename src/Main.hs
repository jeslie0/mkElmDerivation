{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)
import Crypto.Hash.SHA256 (hashlazy)
import Data.Aeson (decode)
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (KeyMap, toList)
import Data.Bifunctor (first)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)
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
    openFile,
  )
import System.TimeIt

output :: FilePath
output = "/home/james/elmNix.nix"

main :: IO ()
main = timeIt $ do
  handle <- openFile output WriteMode
  fileOkay <- (&&) <$> hIsOpen handle <*> hIsWritable handle
  ( if fileOkay
      then
        ( do
            B.hPut handle "{\n"
            mSrc <- src
            case mSrc of
              Nothing -> do
                print "Failed to parse JSON."
                B.hPut handle "}\n"
                hClose handle
              (Just keyMap) -> do
                downloadElmPackages handle . keyMapToList $ keyMap
                B.hPut handle "}\n"
                print "Done."
                hClose handle
        )
      else print "Can't open File"
    )

-- * Elm Data

src :: IO (Maybe (KeyMap Versions))
src = do
  jsonData <- B.readFile "/home/james/Documents/Projects/elmNix/package.elm-lang.org.json"
  return $ decode jsonData

keyMapToList :: KeyMap Versions -> ElmPackages
keyMapToList map =
  uncurry ElmPackage . first (T.fromStrict . toText) <$> toList map

type Version = T.Text

type Versions = [Version]

data ElmPackage = ElmPackage
  { packageName :: T.Text,
    versions :: Versions
  }
  deriving (Eq, Show)

type ElmPackages = [ElmPackage]

data FullElmPackage = FullElmPackage
  { package :: T.Text,
    version :: T.Text,
    hash :: B.ByteString
  }
  deriving (Eq, Show)

makeLink :: T.Text -> Version -> T.Text
makeLink name ver =
  "https://github.com/" <> name <> "/archive/refs/tags/" <> ver <> ".tar.gz"

-- * Hashing Functions

-- | Hash the given bytes and print it nicely.
prettyHash :: B.ByteString -> B.ByteString
prettyHash = toLazyByteString . byteStringHex . hashlazy

-- * Download data

downloadPackage :: Handle -> T.Text -> Version -> IO (Maybe B.ByteString)
downloadPackage handle name ver = do
  print $ "Downloading " <> name <> ": v" <> ver <> "."
  req <- parseRequest . T.unpack $ makeLink name ver
  resp <- httpLBS req
  let status = getResponseStatus resp
  if statusIsSuccessful status
    then do
      print $ "Successfully downloaded " <> name <> "."
      return . Just $ getResponseBody resp
    else do
      print $ "Failed to fetch " <> name <> " " <> ver <> "."
      B.hPut handle "}\n"
      return Nothing

hashElmPackage :: Handle -> T.Text -> Version -> IO (Maybe B.ByteString)
hashElmPackage handle name ver = do
  mBytes <- downloadPackage handle name ver
  print $ "Hashing: " <> name <> ": v" <> ver
  return $ prettyHash <$> mBytes

downloadElmPackage :: Handle -> ElmPackage -> IO [Maybe FullElmPackage]
downloadElmPackage handle (ElmPackage name versions) =
  mapM
    ( \ver -> do
        mHash <- hashElmPackage handle name ver
        case mHash of
          Nothing -> do
            return Nothing
          (Just hash) -> do
            B.hPutStr handle $ toNix $ FullElmPackage name ver hash
            hFlush handle
            return . Just $ FullElmPackage name ver hash
    )
    versions

downloadElmPackages :: Handle -> ElmPackages -> IO [Maybe FullElmPackage]
downloadElmPackages handle pkgs = do
  foo <- mapM (downloadElmPackage handle) pkgs
  return . join $ foo

-- To Nix function
toNix :: FullElmPackage -> B.ByteString
toNix (FullElmPackage name ver hash) =
  let nameString = encodeUtf8 name
      verString = encodeUtf8 ver
   in "    \"" <> nameString <> "\".\"" <> verString <> "\" = \"" <> hash <> "\";\n"
