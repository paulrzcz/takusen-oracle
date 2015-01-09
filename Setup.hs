#!/usr/bin/env runhaskell 

-- setup configure -fodbc -foracle -fpostgres -fsqlite

import Prelude hiding (catch)
import Distribution.PackageDescription
  ( PackageDescription(..), Library(..), BuildInfo(..), HookedBuildInfo
  , emptyHookedBuildInfo, emptyBuildInfo
  )
import Distribution.PackageDescription.Parse ( writeHookedBuildInfo ) 
import Distribution.Package (Dependency(..), PackageName(..))
import Distribution.Simple.Setup ( ConfigFlags(..), BuildFlags(.. ), fromFlag)
import Distribution.Simple
  ( defaultMainWithHooks, autoconfUserHooks, UserHooks(..), Args )
import Distribution.Simple.Program (findProgramOnPath, simpleProgram, Program(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Utils (warn, info, rawSystemStdout)
import Distribution.Verbosity (Verbosity)

import qualified System.Info (os)
import System.Directory (canonicalizePath, removeFile, doesDirectoryExist)
import System.Environment (getEnv)
import System.FilePath (combine, dropFileName, FilePath, pathSeparators, (</>))
import Control.Monad(liftM)
import Control.Exception (SomeException, try, catch)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)

{-
One install script to rule them all, and in the darkness build them...

See this page for useful notes on tagging and releasing:
  http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program

To-dos for Takusen:
 - Oracle resource leak: Ref Cursors (StmtHandles) not freed
 - better result-set <-> iteratee validation. Check column types?
 - use hsc2hs to create #define constants from header files,
   rather than hard-code them.
 - Blob support (and clob?).
 
 - Unwritten tests:
   * incorrect fold function (doesn't match result-set)

-}

emptyCatchHandler :: SomeException -> IO ()
emptyCatchHandler _ = return ()

main = defaultMainWithHooks autoconfUserHooks
  { preConf=preConf, postConf=postConf
  , hookedPrograms = [sqlplusProgram]
  }
  where
    preConf :: Args -> ConfigFlags -> IO HookedBuildInfo
    preConf args flags = do
      catch (removeFile "Takusen.buildinfo") emptyCatchHandler
      return emptyHookedBuildInfo
    postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConf args flags pkgdesc localbuildinfo = do
      let verbose = fromFlag (configVerbosity flags)
      let lbi = libBuildInfo (fromJust (library pkgdesc))
      let buildtools = buildTools lbi
      oraBI <- configOracle verbose buildtools
      let bi = mconcat [oraBI, lbi]
      writeHookedBuildInfo "Takusen.buildinfo" (Just bi, [])

sqlplusProgram    = simpleProgram "sqlplus"

isWindows = System.Info.os == "mingw32" || System.Info.os == "windows"

-- ghc-6.6.1 can't cope with a trailing slash or backslash on the end
-- of the include dir path, so we strip it off.
-- Not sure why this is; there might be something else causing it to fail
-- which has gone unnoticed.
stripTrailingSep :: String -> String
stripTrailingSep p
  = reverse
  . (\s -> if [head s] `isInfixOf` pathSeparators then drop 1 s else s)
  . reverse
  $ p

makeConfig path libDir includeDir = do
  libDirs <- canonicalizePath (combine path libDir)
  includeDirs <- canonicalizePath (combine path includeDir)
  return
    (emptyBuildInfo
      { extraLibDirs = [stripTrailingSep libDirs], includeDirs = [stripTrailingSep includeDirs] })

nothingForSomeException :: SomeException -> IO (Maybe String)
nothingForSomeException _ = return Nothing

maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv env = catch (liftM Just (getEnv env)) nothingForSomeException

-- Check that the program is in the buildtools.
-- If it is, then run the action (which should return BuildInfo).
-- If not, return emptyBuildInfo.
-- Cabal populates the buildtools list depending on which flags
-- have been passed to "setup configure".
guardProg :: Program -> [Dependency] -> IO BuildInfo -> IO BuildInfo
guardProg prog tools action = if prog `isElem` tools then action else return emptyBuildInfo
  where
    isElem program = any (match program)
    match program (Dependency (PackageName tool) _) = programName program == tool

-- Run the first action to give a Maybe FilePath.
-- If this is Nothing then emit a warning about library not found.
-- Otherwise, run the second action over the FilePath.
guardPath :: IO (Maybe FilePath) -> String -> Verbosity -> (FilePath -> IO BuildInfo) -> IO BuildInfo
guardPath pathAction libName verbose resAction = do
  putStrLn $ "Looking for "++libName++"..."
  mb <- pathAction
  case mb of
    Nothing -> warn verbose ("No " ++libName++ " library found") >> return emptyBuildInfo
    Just path -> info verbose ("Using " ++libName++ ": " ++ path) >> resAction path

-- From the Oracle 10g manual:
--
-- Appendix D - Getting Started with OCI for Windows:
--   Compiling OCI Applications for Windows:
-- http://download.oracle.com/docs/cd/B19306_01/appdev.102/b14250/ociadwin.htm#i634569
-- Header files are in: ORACLE_BASE\ORACLE_HOME\oci\include
-- DLLs are in: ORACLE_BASE\ORACLE_HOME\bin
--
-- For Unix full install:
-- Appendix B - OCI Demonstration Programs:
-- http://download.oracle.com/docs/cd/B19306_01/appdev.102/b14250/ociabdem.htm#i459676
-- Header files are in: $ORACLE_HOME/rdbms/public
-- Object files are in: $ORACLE_HOME/lib
--
-- For Unix client only install:
-- Header files are in: $ORACLE_HOME/oci/include
-- Object files are in: $ORACLE_HOME

configOracle verbose buildtools =
  guardProg sqlplusProgram buildtools $
  guardPath (maybeGetEnv "ORACLE_HOME") "Oracle" verbose $ \path -> do
  let fullInstallDirs = ("lib", "rdbms/public")
  isFullInstall <- doesDirectoryExist (path </> snd fullInstallDirs)
  let (libDir, incDir)
        | isWindows = ("bin", "oci/include")
        | isFullInstall = fullInstallDirs
        | otherwise = ("", "sdk/include")
  makeConfig path libDir incDir

