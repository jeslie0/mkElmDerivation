{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GetPackages where

import Conduit
import Control.Concurrent
import Control.Concurrent.Pool
import Control.Monad.Reader
import Crypto.Hash
import qualified Crypto.Hash as C
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import MapHelpers
import Types
import Network.HTTP.Conduit
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
hashConduit :: (HashAlgorithm a, Monad m, ByteArrayAccess i) => ConduitT i Void m (Digest a)
hashConduit = go hashInit
  where
    go :: (Monad m, ByteArrayAccess i, HashAlgorithm a) => Context a -> ConduitT i Void m (Digest a)
    go ctx = do
      bytesM <- await
      case bytesM of
        Just bytes -> go $! hashUpdate ctx bytes
        Nothing -> return $! hashFinalize ctx

-- | Fetch and hash the given request in constant memory, using conduits.
fetchAndHash :: (MonadResource m, Monad m) => Request -> Manager -> m (Maybe T.Text)
fetchAndHash request manager = do
  resp <- http request manager
  if statusIsSuccessful . responseStatus $ resp
    then do
      let conduit = responseBody resp
      digest <- runConduit $ conduit .| (hashConduit :: (Monad m) => ConduitT BS.ByteString Void m (Digest SHA256))
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
  archiveRequest <- parseRequest . T.unpack $ makeLink name ver
  docsRequest <- parseRequest . T.unpack $ makeDocsLink name ver
  
  archiveHashM <- runResourceT $ fetchAndHash archiveRequest manager
  docsHashM <- runResourceT $ fetchAndHash docsRequest manager
  
  (ReadState sucMvar failMvar) <- ask
  case (archiveHashM, docsHashM) of
    (Just archiveHash, Just docsHash) -> do
      succMap <- liftIO $ takeMVar sucMvar
      let packageHashes = PackageHashes archiveHash docsHash
      liftIO $ putMVar sucMvar (addHashesToMap elmPkg packageHashes succMap)
    _ -> do
      -- Add to failed pkgs mvar
      failMap <- liftIO $ takeMVar failMvar
      liftIO $ putMVar failMvar (M.insertWith (<>) name [ver] failMap)

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
