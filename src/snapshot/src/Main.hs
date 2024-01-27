{- This code is mostly taken from elm2nix, with some of my own
modifications to integrate it into this project. As such, I need to
provide a copy of elm2nix's license.

Copyright Domen Kožar (c) 2017-2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Domen Kožar nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad (liftM2, liftM3, (<=<))
import Data.Aeson qualified as Aeson
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Binary qualified as Binary
import Data.Binary.Get.Internal (readN)
import Data.Binary.Put (putBuilder)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word16)
import System.Directory ( makeAbsolute )

data Name = Name
  { _author :: !Text,
    _project :: !Text
  }
  deriving (Eq, Ord)

data Package = Package
  { _name :: !Name,
    _version :: !Version
  }
  deriving (Eq, Ord)

data Version = Version
  { _major :: {-# UNPACK #-} !Word16,
    _minor :: {-# UNPACK #-} !Word16,
    _patch :: {-# UNPACK #-} !Word16
  }
  deriving (Eq, Ord)

data KnownVersions = KnownVersions
  { _newest :: Version,
    _previous :: ![Version]
  }

data Registry = Registry
  { _count :: !Int,
    _versions :: !(Map Name KnownVersions)
  }

putUnder256 :: BS.ByteString -> Binary.Put
putUnder256 bs =
  do
    putWord8 (fromIntegral (BS.length bs))
    putBuilder (BS.byteString bs)

getUnder256 :: Binary.Get BS.ByteString
getUnder256 =
  do
    word <- getWord8
    let !n = fromIntegral word
    readN n id

instance Binary Name where
  get =
    liftM2
      Name
      (fmap Text.decodeUtf8 getUnder256)
      (fmap Text.decodeUtf8 getUnder256)

  put (Name author project) =
    do
      putUnder256 (Text.encodeUtf8 author)
      putUnder256 (Text.encodeUtf8 project)

instance Binary Package where
  get =
    liftM2 Package get get

  put (Package name version) =
    do
      put name
      put version

instance Binary Version where
  get =
    do
      word <- getWord8
      if word == 255
        then liftM3 Version get get get
        else do
          minor <- fmap fromIntegral getWord8
          patch <- fmap fromIntegral getWord8
          return (Version (fromIntegral word) minor patch)

  put (Version major minor patch) =
    if major < 256 && minor < 256 && patch < 256
      then do
        putWord8 (fromIntegral major)
        putWord8 (fromIntegral minor)
        putWord8 (fromIntegral patch)
      else do
        putWord8 255
        put major
        put minor
        put patch

instance Binary KnownVersions where
  get = liftM2 KnownVersions get get
  put (KnownVersions a b) = put a >> put b

instance Binary Registry where
  get = liftM2 Registry get get
  put (Registry a b) = put a >> put b

main :: IO ()
main = do
  packagesJson <- BS.readFile <=< makeAbsolute $ "./all-packages.json"
  let packages = unwrap $ case Aeson.decodeStrict packagesJson of
        Nothing -> error "Couldn't parse all-packages.json"
        Just val -> val
      size = Map.foldr' addEntry 0 packages
      registry = Registry size packages

      addEntry :: KnownVersions -> Int -> Int
      addEntry (KnownVersions _ vs) count =
        count + 1 + length vs

  Binary.encodeFile "registry.dat" registry

newtype Packages = Packages {unwrap :: Map.Map Name KnownVersions}

toKnownVersions :: Map.Map Name [Version] -> Map.Map Name KnownVersions
toKnownVersions =
  fmap
    ( \versions ->
        case List.sortBy (flip compare) versions of
          v : vs -> KnownVersions v vs
          [] -> undefined
    )

instance Aeson.FromJSON Packages where
  parseJSON v = Packages <$> fmap toKnownVersions (Aeson.parseJSON v)

instance Aeson.FromJSON Version where
  parseJSON = Aeson.withText "string" $ \x ->
    case Text.splitOn "." x of
      [major, minor, patch] ->
        return $
          Version
            (read (Text.unpack major))
            (read (Text.unpack minor))
            (read (Text.unpack patch))
      _ ->
        fail "failure parsing version"

instance Aeson.FromJSON Name where
  parseJSON = Aeson.withText "string" $ \x ->
    case Text.splitOn "/" x of
      [author, package] -> return $ Name author package
      lst -> fail $ "wrong package name: " <> show lst

instance Aeson.FromJSONKey Name where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ \x ->
    case Text.splitOn "/" x of
      [author, package] -> return $ Name author package
      lst -> fail $ "wrong package name: " <> show lst
