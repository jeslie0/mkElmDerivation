{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as B
import Conduit
import qualified Data.Conduit.Binary as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import MkElmDerivation.Conduits
import MkElmDerivation.Types

-- * Main

main :: IO (Maybe ())
main = runMaybeT $ do
  -- Use a conduit to get the (potentially non-existent) file of
  -- hashed packages.
  outputPath <- liftIO output
  alreadyHashedPkgs <- liftIO . runConduitRes $ catchC (BS.sourceFile outputPath) (\(e :: IOException) -> liftIO $ print e) .| conduitFile2Map

  -- Use a conduit to get the (potentially non-existent) file of
  -- failed packages.
  failuresPath <- liftIO failures
  previousFailuresNVsMap <- liftIO . runConduitRes $ catchC (BS.sourceFile failuresPath) (\(e :: IOException) -> liftIO $ print e) .| conduitFile2Map

  -- The keymap obtained from downloading and parsing
  -- elm-packages.json.
  elmJsonNVsMap <- remoteSrc

  pkgsToHash <- liftIO $ getNewPkgsToHash alreadyHashedPkgs previousFailuresNVsMap elmJsonNVsMap

  (newFailures, newSuccesses) <- liftIO $ getFailuresSuccesses pkgsToHash

  liftIO $ runConduitRes $ conduitSaveSuccessesMap newSuccesses alreadyHashedPkgs .| BS.sinkFile outputPath
  liftIO $ runConduitRes $ conduitSaveFailuresMap newFailures previousFailuresNVsMap .| BS.sinkFile failuresPath

  liftIO $ printReport previousFailuresNVsMap alreadyHashedPkgs elmJsonNVsMap pkgsToHash newFailures newSuccesses

-- * Input and Output Files

-- | Prints a report about the past and current packages.
printReport ::
  -- | Previous failures
  M.HashMap Name Versions ->
  -- | Previously hashed packages
  M.HashMap Name (M.HashMap Version Hash) ->
  -- | Packages in elm-packages.json
  M.HashMap Name Versions ->
  -- | Packages we are hashing
  M.HashMap Name Versions ->
  -- | New Failures
  M.HashMap Name Versions ->
  -- | New packages hashed
  M.HashMap Name (M.HashMap Version Hash) ->
  IO ()
printReport prevFails prevHashed elmPkgs toHash newFails newSuccesses = do
  putStrLn $ "Number of previous failed packages: " ++ show (sumOfMap prevFails) ++ "."
  putStrLn $ "Number of previous hashed packages: " ++ show (sumOfMap prevHashed) ++ "."
  putStrLn $ "Number of elm packages: " ++ show (sumOfMap elmPkgs) ++ "."
  putStrLn $ "Number of new packages to hash: " ++ show (sumOfMap toHash) ++ "."
  putStrLn $ "Number of new packages which failed: " ++ show (sumOfMap newFails) ++ "."
  putStrLn $ "Number of new packages hashed: " ++ show (sumOfMap newSuccesses) ++ "."
  where
    sumOfMap :: (Foldable t) => M.HashMap a (t b) -> Int
    sumOfMap = M.foldl' (\a b -> a + length b) 0
