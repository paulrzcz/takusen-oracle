
-- |
-- Module      :  Main
-- Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  oleg@pobox.com, alistair@abayley.org
-- Stability   :  experimental
-- Portability :  non-portable
--  
-- Simple driver module, mainly for testing.
-- Imports test modules and runs test suites.
--  
-- This project is now hosted at haskell.org:
--  
-- @darcs get <http://darcs.haskell.org/takusen>@
--  
-- Invoke main like this (assuming the compiled executable is called @takusen@):
--  
--  > takusen noperf
--  > takusen noperf "" "" dbname  -- no username, so os-authenticated
--  > takusen noperf user paswd dbname

module Main (main) where

import Database.Oracle.Test.Enumerator as Oracle
import System.Environment (getArgs)
import Database.Test.Performance as Perf
import Database.Test.Util as Util
import Foreign.C.Test.UTF8 as UTF8


main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then showUsage
    else do
      let (perf:as) = args
      doTests perf as

showUsage = putStrLn "usage: takusen backend {no}perf user pswd database"

doTests perf args = do
  let runPerf = if perf == "perf" then Perf.RunTests else Perf.Don'tRunTests
  Oracle.runTest runPerf args
