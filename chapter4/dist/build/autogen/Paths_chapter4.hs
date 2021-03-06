module Paths_chapter4 (
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
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/carminemoleti/.cabal/bin"
libdir     = "/Users/carminemoleti/.cabal/lib/x86_64-osx-ghc-7.6.3/chapter4-0.0.1"
datadir    = "/Users/carminemoleti/.cabal/share/x86_64-osx-ghc-7.6.3/chapter4-0.0.1"
libexecdir = "/Users/carminemoleti/.cabal/libexec"
sysconfdir = "/Users/carminemoleti/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chapter4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chapter4_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "chapter4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chapter4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chapter4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
