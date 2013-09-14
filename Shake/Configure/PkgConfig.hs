{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | pkg-config support functions
module Shake.Configure.PkgConfig where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Attoparsec.Text
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P
import           Shake.Configure.Finder
import           Shake.Configure.Package
import           Shelly

-- | A package finder method using pkg-config.
pkgConfigFind :: String -> FindMethod PackageConfig
pkgConfigFind = FindMethod . MaybeT . lookupPackage . T.pack

-- | Use @pkg-config@ to get information about a package. Returns @Nothing@ if the package isn't found.
lookupPackage :: T.Text -> IO (Maybe PackageConfig)
lookupPackage package = shellyNoDir $ silently $ do
  let makeFlags = filter (not . T.null) . map T.strip . T.splitOn " "
  compilerFlags <- cmd "pkg-config" "--cflags" package
  linkerFlags <- cmd "pkg-config" "--libs" package
  vars <- lookupVariables package
  v <- cmd "pkg-config" "--modversion" package
  static <- makeFlags <$> cmd "pkg-config" "--static" package  
  return $ fmap ( set variables vars
                . set version (parseOnly versionP v ^? _Right)
                . set staticFlags static
                ) $ parseCompilerFlags compilerFlags
                <>  parseLinkerFlags linkerFlags

-- | Use @pkg-config@ to get the variables set by a package.
lookupVariables :: T.Text -> Sh (M.Map T.Text T.Text)
lookupVariables package = do
  names <- T.lines <$> cmd "pkg-config" "--print-variables" package
  values <- traverse (lookupVariable package) names
  return $ M.fromList $ zip names values

-- | Get the value of a pkg-config variable
lookupVariable :: T.Text -> T.Text -> Sh T.Text
lookupVariable package variable = T.strip <$> cmd "pkg-config" "--variable" variable package

-- | This function parses the cflags output of pkg-config. It only threats -I and -i flags special. The remaining
-- flags are just added to 'cflags'.
parseCompilerFlags :: T.Text -> Maybe PackageConfig
parseCompilerFlags = preview _Right . fmap mconcat . parseOnly (f `sepBy` many1 space)
  where define = mapItem defines . T.breakOn "=" <$> flag 'D'
        incPath = dir includePaths <$> flag 'I'
        other = item cflags <$> takeWhile1 (/= ' ')
        f = try incPath <|> try define <|> other

-- | This function parses the lflags output of pkg-config. It only threats -L and -l flags special. The remaining
-- flags are simply added to 'lflags'.
parseLinkerFlags :: T.Text -> Maybe PackageConfig
parseLinkerFlags = preview _Right . fmap mconcat . parseOnly (f `sepBy` many1 space)
  where lib = item libraries <$> flag 'l'
        libPath = dir libraryPaths <$> flag 'L'
        other = item cflags <$> takeWhile1 (/= ' ')
        f = try lib <|> try libPath <|> other

-- | Parse a command line flag
-- FIXME: This does not yet handle quotes in arguments
flag :: Char -> Parser T.Text
flag c = string ("-" <> T.singleton c) *> optional space *> takeWhile1 (/= ' ')

-- | Construct a 'PackageConfig' with a single directory list field.
dir :: Lens' PackageConfig [P.FilePath] -> T.Text -> PackageConfig
dir l = item l . P.fromText

-- | Construct a 'PackageConfig' with a single list field.
item :: Lens' PackageConfig [a] -> a -> PackageConfig
item l x = reviewEmpty l [x]

-- | Construct a 'PackageConfig' with a single map entry
mapItem :: Ord a => Lens' PackageConfig (M.Map a b) -> (a,b) -> PackageConfig
mapItem l x = reviewEmpty l $ M.fromList [x]
