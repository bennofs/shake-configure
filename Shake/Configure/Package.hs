{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Shake.Configure.Package where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types hiding (Parser)
import           Data.Attoparsec.Text
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import           Data.Version

data PackageConfig = PackageConfig
  { _includePaths :: [FilePath]
  , _libraries    :: [String]
  , _version      :: Maybe Version
  , _variables    :: M.Map String String
  , _defines      :: M.Map String String
  , _libraryPaths :: [FilePath]
  , _staticFlags  :: [String]
  , _cflags       :: [String]
  , _lflags       :: [String]
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

deriveJSON defaultOptions
  { fieldLabelModifier = drop 1
  } ''PackageConfig

reviewEmpty :: Monoid s => Lens s t a b -> b -> t
reviewEmpty l v = mempty & l .~ v

getCompilerFlags :: PackageConfig -> [String]
getCompilerFlags config = mconcat
  [ config ^.. includePaths . traverse . flag "-I"
  , config ^.. defines . itraversed . withIndex . to define
  , config ^.. cflags . traverse
  ]
  where flag f g v = coerce $ traverse g [f,v]
        define (k,v) = "-D" <> k <> "=" <> v

getLinkerFlags :: PackageConfig -> [String]
getLinkerFlags config = mconcat
  [ config ^.. libraries . traverse . flag "-l"
  , config ^.. libraryPaths . traverse . flag "-L"
  , config ^.. lflags . traverse
  ]
  where flag f g v = coerce $ traverse g [f,v]

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
