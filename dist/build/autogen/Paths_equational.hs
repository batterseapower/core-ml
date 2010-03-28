module Paths_equational (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/mbolingbroke/.cabal/bin"
libdir     = "/Users/mbolingbroke/.cabal/lib/equational-0.1/ghc-6.12.1"
datadir    = "/Users/mbolingbroke/.cabal/share/equational-0.1"
libexecdir = "/Users/mbolingbroke/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "equational_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "equational_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "equational_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "equational_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
