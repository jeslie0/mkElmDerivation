{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module MkElmDerivation.Types where

import Control.Concurrent
import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics

type Name = T.Text

type Hash = T.Text

type Version = T.Text

type Versions = V.Vector Version

data ElmPackage = ElmPackage
  { packageName :: T.Text,
    version :: Version
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type ElmPackages = V.Vector ElmPackage

data ReadState = ReadState
  { hashedPkgsMvar :: MVar (M.HashMap Name (M.HashMap Version Hash)),
    failedPkgsMvar :: MVar (M.HashMap Name Versions)
  }
