{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GetPackages (downloadElmPackages) where

import Conduit
import Control.Concurrent (modifyMVar_)
import Control.Concurrent.Pool (newPoolIO, noMoreTasksIO, queue, waitForIO)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Crypto.Hash qualified as C
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import MapHelpers (addHashBundleToMap, mapMWithKey_)
import Network.HTTP.Conduit (Manager, Request, Response (..), http, newManager, parseRequest, tlsManagerSettings)
import Network.HTTP.Types.Status (statusIsSuccessful)
import Types

-- | Take a name and a version and output the link to download it
-- from.
makeArchiveLink ::
  -- | Name of Elm package (e.g elm/elm-ui).
  Name ->
  -- | Version of Elm package (e.g 0.1.0)
  Version ->
  T.Text
makeArchiveLink name ver =
  "https://github.com/" <> name <> "/archive/" <> ver <> ".tar.gz"

-- | Take a name and a version and output the link to download the matching
-- docs.json file.
makeDocsLink ::
  -- | Name of Elm package (e.g elm/elm-ui).
  Name ->
  -- | Version of Elm package (e.g 0.1.0)
  Version ->
  T.Text
makeDocsLink name ver =
  "https://package.elm-lang.org/packages/" <> name <> "/" <> ver <> "/docs.json"

-- | A conduit sink to incrementally hash the input, returning the hash.
hashConduit :: (C.HashAlgorithm a, Monad m, ByteArrayAccess i) => ConduitT i Void m (C.Digest a)
hashConduit = go C.hashInit
  where
    go :: (Monad m, ByteArrayAccess i, C.HashAlgorithm a) => C.Context a -> ConduitT i Void m (C.Digest a)
    go ctx = do
      bytesM <- await
      case bytesM of
        Just bytes -> go $! C.hashUpdate ctx bytes
        Nothing -> return $! C.hashFinalize ctx

-- | Fetch and hash the given request in constant memory, using conduits.
fetchAndHash :: (MonadResource m, Monad m) => Request -> Manager -> m (Maybe T.Text)
fetchAndHash request manager = do
  resp <- http request manager
  if statusIsSuccessful . responseStatus $ resp
    then do
      let conduit = responseBody resp
      digest <- runConduit $ conduit .| (hashConduit :: (Monad m) => ConduitT BS.ByteString Void m (C.Digest C.SHA256))
      return . Just . T.pack . show $ digest
    else return Nothing

-- | Download and hash the given elm package, adding the data to the
-- MVars in the ReadState.
downloadElmPackage ::
  -- | The http manager to be fed to the http function
  Manager ->
  -- | The Elm package to operate on.
  ElmPackage ->
  ReaderT ReadState IO ()
downloadElmPackage manager elmPkg@(ElmPackage name ver) = do
  maybeHashBundle <-
    runMaybeT $
      HashBundle <$> hashURLContent (makeArchiveLink name ver) <*> hashURLContent (makeDocsLink name ver)
  (ReadState successMapMvar failMvar) <- ask
  case maybeHashBundle of
    Nothing -> do
      -- Add to failed pkgs mvar
      void . liftIO . modifyMVar_ failMvar $ pure . M.insertWith (<>) name [ver]
    Just !hashBundle -> do
      -- Add to successful pkgs mvar
      void . liftIO . modifyMVar_ successMapMvar $ pure . addHashBundleToMap elmPkg hashBundle
  where
    hashURLContent link = MaybeT $ do
      request <- parseRequest . T.unpack $ link
      runResourceT $ fetchAndHash request manager

-- | Downloads all of the elm packages asynchronously, using 100
-- threads.
downloadElmPackages ::
  -- | Map of package
  -- names and versions to hash.
  M.HashMap Name Versions ->
  ReaderT ReadState IO ()
downloadElmPackages pkgs = do
  state <- ask
  manager <- liftIO $ newManager tlsManagerSettings
  pool <- liftIO $ newPoolIO 100 False
  let elmPkgProcess rDld = runReaderT (downloadElmPackage manager rDld) state
      runElmPkgProcess name vers = mapM_ ((\io -> queue pool io ()) . elmPkgProcess) $ ElmPackage name <$> vers
      fetchPackageBytessInPool = mapMWithKey_ runElmPkgProcess pkgs
  liftIO fetchPackageBytessInPool
  liftIO $ noMoreTasksIO pool
  liftIO $ waitForIO pool
