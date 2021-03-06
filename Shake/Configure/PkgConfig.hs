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
import           Data.Char
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import           Shake.Configure.Finder
import           Shake.Configure.Package
import           Shelly hiding (FilePath)

-- | A package finder method using pkg-config.
pkgConfigFind :: String -> FindMethod PackageConfig
pkgConfigFind = FindMethod . MaybeT . lookupPackage . T.pack

-- | Remove whitespace from beginning and end of a string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Use @pkg-config@ to get information about a package. Returns @Nothing@ if the package isn't found.
lookupPackage :: T.Text -> IO (Maybe PackageConfig)
lookupPackage package = shellyNoDir $ silently $ do
  let makeFlags = filter (not . null) . map (T.unpack . T.strip) . T.splitOn " "
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
lookupVariables :: T.Text -> Sh (M.Map String String)
lookupVariables package = do
  names <- map T.unpack . T.lines <$> cmd "pkg-config" "--print-variables" package
  values <- traverse (lookupVariable package) names
  return $ M.fromList $ zip names $ map T.unpack values

-- | Get the value of a pkg-config variable
lookupVariable :: T.Text -> String -> Sh T.Text
lookupVariable package variable = T.strip <$> cmd "pkg-config" "--variable" variable package

-- | This function parses the cflags output of pkg-config. It only threats -I and -i flags special. The remaining
-- flags are just added to 'cflags'.
parseCompilerFlags :: T.Text -> Maybe PackageConfig
parseCompilerFlags = preview _Right . fmap mconcat . parseOnly (f `sepBy` many1 space)
  where define = mapItem defines . over both T.unpack . T.breakOn "=" <$> flag 'D'
        incPath = dir includePaths . T.unpack <$> flag 'I'
        other = item cflags . T.unpack <$> takeWhile1 (/= ' ')
        f = try incPath <|> try define <|> other

-- | This function parses the lflags output of pkg-config. It only threats -L and -l flags special. The remaining
-- flags are simply added to 'lflags'.
parseLinkerFlags :: T.Text -> Maybe PackageConfig
parseLinkerFlags = preview _Right . fmap mconcat . parseOnly (f `sepBy` many1 space)
  where lib = item libraries . T.unpack <$> flag 'l'
        libPath = dir libraryPaths . T.unpack <$> flag 'L'
        other = item cflags . T.unpack <$> takeWhile1 (/= ' ')
        f = try lib <|> try libPath <|> other

-- | Parse a command line flag
-- FIXME: This does not yet handle quotes in arguments
flag :: Char -> Parser T.Text
flag c = string ("-" <> T.singleton c) *> optional space *> takeWhile1 (/= ' ')

-- | Construct a 'PackageConfig' with a single directory list field.
dir :: Lens' PackageConfig [FilePath] -> String -> PackageConfig
dir = item

-- | Construct a 'PackageConfig' with a single list field.
item :: Lens' PackageConfig [a] -> a -> PackageConfig
item l x = reviewEmpty l [x]

-- | Construct a 'PackageConfig' with a single map entry
mapItem :: Ord a => Lens' PackageConfig (M.Map a b) -> (a,b) -> PackageConfig
mapItem l x = reviewEmpty l $ M.fromList [x]
