{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Conduits where

import Control.Concurrent qualified as Con
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BL
import Data.Conduit
import Data.Conduit.Binary qualified as BS
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Data.Text qualified as T
import Data.Vector qualified as V
import GetPackages
import MapHelpers
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Directory
import Types

-- | The absolute path of the output file, to read and save successes.
output :: IO FilePath
output = makeAbsolute "./mkElmDerivation/elm-hashes.json"

-- | The absolute path of the failures file, to read and save failures.
failures :: IO FilePath
failures = makeAbsolute "./mkElmDerivation/elm-failures.json"

conduitFile2Map ::
  (Monad m, FromJSON b, FromJSONKey a, Hashable a, MonadIO m) =>
  ConduitT B.ByteString Void m (M.HashMap a b)
conduitFile2Map = helper ""
  where
    helper acc = do
      contentM <- await
      case contentM of
        Just bytes -> helper $ acc <> byteString bytes
        Nothing -> do
          case decode . toLazyByteString $ acc of
            Nothing -> return M.empty
            Just niceMap -> return niceMap

conduitSaveFailuresMap ::
  (Monad m) =>
  M.HashMap Name Versions ->
  M.HashMap Name Versions ->
  ConduitT () B.ByteString m ()
conduitSaveFailuresMap failedPackages alreadyFailed = do
  yield . B.toStrict . encode $ updateFailures failedPackages alreadyFailed

conduitSaveSuccessesMap ::
  (Monad m) =>
  M.HashMap Name (M.HashMap Version HashBundle) ->
  M.HashMap Name (M.HashMap Version HashBundle) ->
  ConduitT () B.ByteString m ()
conduitSaveSuccessesMap successfulPackages alreadyHashed = do
  yield . B.toStrict . encode $ joinNewPackages successfulPackages alreadyHashed

-- | Fetch elmPackages JSON and parse into a map.
remoteSrc :: MaybeT IO (M.HashMap Name Versions)
remoteSrc = do
  liftIO $ print "Fetching elmPackages.json"
  req <- parseRequest "https://package.elm-lang.org/all-packages"
  resp <- httpLBS req
  let status = getResponseStatus resp
  if statusIsSuccessful status
    then do
      let body = getResponseBody resp
      liftIO $ BL.writeFile "./mkElmDerivation/all-packages.json" body
      MaybeT . return . decode $ body
    else do
      liftIO $ print "Could not fetch json file"
      MaybeT . return $ Nothing

getNewPkgsToHash ::
  -- | Already hashed packages.
  M.HashMap Name (M.HashMap Version HashBundle) ->
  -- | Previous failed packages
  M.HashMap Name Versions ->
  -- | All elm-packages.json.
  M.HashMap Name Versions ->
  IO (M.HashMap Name Versions)
getNewPkgsToHash alreadyHashed failuresMap allPkgsMap =
  return . removeFailedPkgs (extractNewPackages allPkgsMap alreadyHashed) $ failuresMap

getFailuresSuccesses ::
  M.HashMap Name Versions ->
  IO (M.HashMap Name Versions, M.HashMap Name (M.HashMap Version HashBundle))
getFailuresSuccesses toHash = do
  sucMvar <- Con.newMVar M.empty
  failMvar <- Con.newMVar M.empty

  runReaderT (downloadElmPackages toHash) $ ReadState sucMvar failMvar

  Con.withMVar failMvar $
    \failedPackages -> Con.withMVar sucMvar $
      \successfulPackages ->
        return (failedPackages, successfulPackages)
