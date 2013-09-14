{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shake.Configure.Finder
  ( FindMethod (..)
  , Finder
  , tryFindMethod
  , runFinder
  , finderPackage
  , defPackage
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Monoid
import Shake.Configure.Package

-- | A find method specifies a way to find a package. It can fail using Nothing, or succeed with a value of type a. Multiple
-- FindMethods can be combined with the Monoid, Alternative or MonadPlus instance. The combined find method will try the first
-- method, and if it fails, try the second one. FindMethods can also make use of IO by lifing IO actions with 'liftIO'.
newtype FindMethod a = FindMethod { runFindMethod :: MaybeT IO a } deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO)

-- | Try a given method to find a package. Returns Nothing if the package could not be found.
tryFindMethod :: FindMethod a -> IO (Maybe a)
tryFindMethod = runMaybeT . runFindMethod

-- | The monoid instance behaves the same as the Alternative/MonadPlus instance for Maybe.
instance Monoid (FindMethod a) where
  mempty = FindMethod $ MaybeT (return Nothing)
  mappend = (<|>)

type Finder = (String, FindMethod PackageConfig)

-- | Run a finder. Returns Nothing if the package could not be found.
runFinder :: Finder -> IO (Maybe PackageConfig)
runFinder (_, method) = tryFindMethod method

-- | Get the name of the package that a given finder tries to find.
finderPackage :: Finder -> String
finderPackage (name, _) = name

-- | Define a new package finder.
defPackage :: String                   -- ^ Name of the package to define
           -> FindMethod PackageConfig -- ^ Method(s) to use for finding the package
           -> Finder
defPackage = (,)
