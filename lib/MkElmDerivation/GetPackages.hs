{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MkElmDerivation.GetPackages where

import Control.Concurrent
import Control.Concurrent.Pool
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Crypto.Hash as C
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import MkElmDerivation.MapHelpers
import MkElmDerivation.Types
import Network.HTTP.Simple
import Network.HTTP.Types.Status

-- | Take a name and a version and output the link to download it
-- from.
makeLink ::
  -- | Name of Elm package (e.g elm/elm-ui).
  Name ->
  -- | Version of Elm package (e.g 0.1.0)
  Version ->
  T.Text
makeLink name ver =
  "https://github.com/" <> name <> "/archive/" <> ver <> ".tar.gz"

-- | Hash the given bytes and print it nicely.
prettyHash ::
  -- | SHA256 these bytes.
  B.ByteString ->
  Hash
prettyHash bytes = T.pack . show $ (C.hashlazy bytes :: C.Digest C.SHA256)

-- | Download the specified elm package into a bytestring.
fetchPackageBytes ::
  -- | Elm package to download.
  ElmPackage ->
  -- | Package wrapped up as bytes.
  MaybeT IO B.ByteString
fetchPackageBytes (ElmPackage name ver) = do
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

-- | Download and hash the given elm package, adding the data to the
-- MVars in the ReadState.
downloadElmPackage ::
  -- | The Elm package to operate on.
  ElmPackage ->
  ReaderT ReadState IO ()
downloadElmPackage elmPkg@(ElmPackage name ver) = do
  bytesM <- liftIO . runMaybeT $ fetchPackageBytes elmPkg
  let hashM = prettyHash <$> bytesM
  (ReadState sucMvar failMvar) <- ask
  case hashM of
    Nothing -> do
      -- Add to failed pkgs mvar
      failMap <- liftIO $ takeMVar failMvar
      liftIO $ putMVar failMvar (M.insertWith (<>) name [ver] failMap)
    Just !hash -> do
      -- Add to successful pkgs mvar
      succMap <- liftIO $ takeMVar sucMvar
      liftIO $ putMVar sucMvar (addHashToMap elmPkg hash succMap)

-- | Downloads all of the elm packages asynchronously, using 100
-- threads.
downloadElmPackages ::
  -- | Map of package
  -- names and versions to hash.
  M.HashMap Name Versions ->
  ReaderT ReadState IO ()
downloadElmPackages pkgs = do
  state <- ask
  pool <- liftIO $ newPoolIO 100 False
  let elmPkgProcess rDld = runReaderT (downloadElmPackage rDld) state
      runElmPkgProcess name vers = mapM_ ((\io -> queue pool io ()) . elmPkgProcess) $ ElmPackage name <$> vers
      fetchPackageBytessInPool = mapMWithKey_ runElmPkgProcess pkgs
  liftIO fetchPackageBytessInPool
  liftIO $ noMoreTasksIO pool
  liftIO $ waitForIO pool
