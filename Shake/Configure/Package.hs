{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Shake.Configure.Package where

import Data.Monoid
import Control.Lens
import Data.Version
import Data.Aeson
import Data.Aeson.Types hiding (Parser)
import Data.Aeson.TH
import Data.Attoparsec.Text
import Control.Applicative
import qualified Filesystem.Path.CurrentOS as P
import qualified Data.Text as T
import qualified Data.Map as M

data PackageConfig = PackageConfig
  { _includePaths :: [P.FilePath]
  , _libraries    :: [T.Text]
  , _version      :: Maybe Version
  , _variables    :: M.Map T.Text T.Text
  , _defines      :: M.Map T.Text T.Text
  , _libraryPaths :: [P.FilePath]
  , _staticFlags  :: [T.Text]
  , _cflags       :: [T.Text]
  , _lflags       :: [T.Text]
  } deriving (Show)
makeLenses ''PackageConfig

versionP :: Parser Version
versionP = Version <$> decimal `sepBy` char '.' <*> fmap (map T.unpack) (char '-' *> takeWhile1 (/= '-') `sepBy1` char '-')

instance ToJSON Version where toJSON = String . T.pack . show
instance FromJSON Version where
  parseJSON (String a) = case parseOnly versionP a of
    Right v -> pure v
    _ -> fail $ "Invalid version string: " <> T.unpack a
  parseJSON o = typeMismatch "version string" o

instance ToJSON P.FilePath where toJSON = String . either (error "Failed to decode file path") id . P.toText
instance FromJSON P.FilePath where parseJSON = withText "file path string" $ pure . P.fromText

deriveJSON defaultOptions
  { fieldLabelModifier = drop 1
  } ''PackageConfig

reviewEmpty :: Monoid s => Lens s t a b -> b -> t
reviewEmpty l v = mempty & l .~ v

instance Monoid PackageConfig where
  mempty = PackageConfig mempty mempty Nothing mempty mempty mempty mempty mempty mempty
  mappend a b =
    a & includePaths <>~ b ^. includePaths
      & libraries <>~ b ^. libraries
      & defines <>~ b ^. defines
      & libraryPaths <>~ b ^. libraryPaths
      & staticFlags <>~ b ^. staticFlags
      & cflags <>~ b ^. cflags
      & lflags <>~ b ^. lflags
      & variables <>~ b ^. variables
      & version . wrapping First <>~ b ^. version . wrapping First
