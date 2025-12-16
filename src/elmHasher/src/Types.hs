{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Concurrent (MVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict as M
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)

type Name = T.Text

type Hash = T.Text

type Version = T.Text

type Versions = V.Vector Version

data ElmPackage = ElmPackage
  { packageName :: !T.Text,
    version :: !Version
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type ElmPackages = V.Vector ElmPackage

data HashBundle = HashBundle {archiveHash :: !Hash, docsHash :: !Hash}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ReadState = ReadState
  { hashedPkgsMvar :: MVar (M.HashMap Name (M.HashMap Version HashBundle)),
    failedPkgsMvar :: MVar (M.HashMap Name Versions)
  }
