{-# LANGUAGE TemplateHaskell #-}
module Shake.Configure
 ( findPackages
 , withPackage
   -- * Reexported
 , module X
 , pkgConfigFind
 ) where

import           Control.Lens hiding (Action)
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Foldable (for_)
import           Development.Shake
import           Shake.Configure.Finder as X
import           Shake.Configure.Package as X
import           Shake.Configure.PkgConfig

newtype Config = Config
  { _cachePath :: FilePath
  }
makeLenses ''Config

-- | Add rules for running the given finders.
findPackages :: FilePath      -- ^ Path under which to cache the configure results
             -> [Finder]      -- ^ Finders to run
             -> Rules Config  -- ^ Configuration handle
findPackages cachePrefix pkgs = fmap (const $ Config cachePrefix) $ for_ pkgs $ \(pkgName, findPkg) ->
  cachePrefix ++ "/" ++ pkgName ++ ".info" *> \out -> do
    putLoud $ "# Configure| Looking for " ++ pkgName
    x <- liftIO $ tryFindMethod findPkg
    case x of
      Just x' -> do
        putLoud $ "# Configure| Found " ++ pkgName
        writeFileChanged out (U.toString $ encode x')
      _ -> error $ "# Configure| " ++ pkgName ++ " not found"

-- | Access a field of a package. Throws an error if the package wasn't found.
withPackage :: Config              -- ^ A config handle obtained by calling 'findPackages'
            -> String              -- ^ Name of the package
            -> (PackageConfig -> a) -- ^ Function to get a field
            -> Action a
withPackage conf name f = do
  c <- readFile' $ conf ^. cachePath ++ "/" ++ name ++ ".info"
  return $ maybe (error $ "# Configure| Could not parse cached settings for " ++ name ++ " invalid") f $ decode $ U.fromString c
