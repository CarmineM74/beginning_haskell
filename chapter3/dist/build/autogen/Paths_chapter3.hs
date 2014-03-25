module Paths_chapter3 (
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
libdir     = "/Users/carminemoleti/.cabal/lib/x86_64-osx-ghc-7.6.3/chapter3-0.0.1"
datadir    = "/Users/carminemoleti/.cabal/share/x86_64-osx-ghc-7.6.3/chapter3-0.0.1"
libexecdir = "/Users/carminemoleti/.cabal/libexec"
sysconfdir = "/Users/carminemoleti/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chapter3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chapter3_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "chapter3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chapter3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chapter3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
