module Paths_courseography (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/christinemurad/Library/Haskell/bin"
libdir     = "/Users/christinemurad/Library/Haskell/ghc-7.8.3-x86_64/lib/courseography-0.1.0.0"
datadir    = "/Users/christinemurad/Library/Haskell/share/ghc-7.8.3-x86_64/courseography-0.1.0.0"
libexecdir = "/Users/christinemurad/Library/Haskell/libexec"
sysconfdir = "/Users/christinemurad/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "courseography_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "courseography_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "courseography_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "courseography_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "courseography_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
