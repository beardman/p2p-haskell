{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_chapter1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/pedropereira/Dev/p2p-haskell/chapter1/.stack-work/install/x86_64-osx/lts-8.6/8.0.2/bin"
libdir     = "/Users/pedropereira/Dev/p2p-haskell/chapter1/.stack-work/install/x86_64-osx/lts-8.6/8.0.2/lib/x86_64-osx-ghc-8.0.2/chapter1-0.1.0.0-7m1R2oEzlyZ3rGhUDuN1Tk"
dynlibdir  = "/Users/pedropereira/Dev/p2p-haskell/chapter1/.stack-work/install/x86_64-osx/lts-8.6/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/pedropereira/Dev/p2p-haskell/chapter1/.stack-work/install/x86_64-osx/lts-8.6/8.0.2/share/x86_64-osx-ghc-8.0.2/chapter1-0.1.0.0"
libexecdir = "/Users/pedropereira/Dev/p2p-haskell/chapter1/.stack-work/install/x86_64-osx/lts-8.6/8.0.2/libexec"
sysconfdir = "/Users/pedropereira/Dev/p2p-haskell/chapter1/.stack-work/install/x86_64-osx/lts-8.6/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chapter1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chapter1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chapter1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chapter1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chapter1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chapter1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)