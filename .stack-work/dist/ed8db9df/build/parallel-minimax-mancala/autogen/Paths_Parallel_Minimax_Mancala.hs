{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Parallel_Minimax_Mancala (
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

bindir     = "C:\\Users\\chenc\\Desktop\\PFP\\project\\Parallel-Minimax-Mancala\\.stack-work\\install\\557fc685\\bin"
libdir     = "C:\\Users\\chenc\\Desktop\\PFP\\project\\Parallel-Minimax-Mancala\\.stack-work\\install\\557fc685\\lib\\x86_64-windows-ghc-8.10.7\\Parallel-Minimax-Mancala-0.1.0.0-LjooabFf7Oz754Ci4ezdAZ-parallel-minimax-mancala"
dynlibdir  = "C:\\Users\\chenc\\Desktop\\PFP\\project\\Parallel-Minimax-Mancala\\.stack-work\\install\\557fc685\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\chenc\\Desktop\\PFP\\project\\Parallel-Minimax-Mancala\\.stack-work\\install\\557fc685\\share\\x86_64-windows-ghc-8.10.7\\Parallel-Minimax-Mancala-0.1.0.0"
libexecdir = "C:\\Users\\chenc\\Desktop\\PFP\\project\\Parallel-Minimax-Mancala\\.stack-work\\install\\557fc685\\libexec\\x86_64-windows-ghc-8.10.7\\Parallel-Minimax-Mancala-0.1.0.0"
sysconfdir = "C:\\Users\\chenc\\Desktop\\PFP\\project\\Parallel-Minimax-Mancala\\.stack-work\\install\\557fc685\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Parallel_Minimax_Mancala_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Parallel_Minimax_Mancala_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Parallel_Minimax_Mancala_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Parallel_Minimax_Mancala_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Parallel_Minimax_Mancala_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Parallel_Minimax_Mancala_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
