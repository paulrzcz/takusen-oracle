
|
Module      :  Main
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple driver module, mainly for testing.
Imports test modules and runs test suites.
 
This project is now hosted at haskell.org:
 
@darcs get <http://darcs.haskell.org/takusen>@
 
Invoke main like this (assuming the compiled executable is called @takusen@):
 
 > takusen stub noperf
 > takusen sqlite noperf "" "" dbname
 > takusen oracle noperf "" "" dbname  -- no username, so os-authenticated
 > takusen mssql noperf user paswd dbname


> module Main (main) where


> import Database.Sqlite.Test.Enumerator as Sqlite
> import Database.Oracle.Test.Enumerator as Oracle
> --import Database.Test.MultiConnect as Multi
> import Database.ODBC.Test.Enumerator as ODBC
> import Database.Stub.Test.Enumerator as Stub
> --import Database.MSSqlServer.Test.Enumerator as MSSql
> import Database.PostgreSQL.Test.Enumerator as PGSql
> import System.Environment (getArgs)
> import Database.Test.Performance as Perf
> import Database.Test.Util as Util
> import Foreign.C.Test.UTF8 as UTF8


> main :: IO ()
> main = do
>   args <- getArgs
>   if (length args < 2)
>     then showUsage
>     else do
>       let (impl:perf:as) = args
>       doTests impl perf as

> showUsage = do
>   putStrLn "usage: takusen backend {no}perf user pswd database"

> doTests impl perf args = do
>   let runPerf = if perf == "perf" then Perf.RunTests else Perf.Don'tRunTests
>   case lookup impl backendTests of
>     Nothing -> putStrLn $ "No backend for " ++ impl ++ "."
>     Just test -> test runPerf args

> backendTests :: [(String, Perf.ShouldRunTests -> [String] -> IO ())]
> backendTests =
>   [ ("sqlite", Sqlite.runTest)
>   , ("pgsql", PGSql.runTest)
>   , ("odbc", ODBC.runTest)
>   --, ("mssql", MSSql.runTest)
>   , ("oracle", Oracle.runTest)
>   --, ("multi", Multi.runTest)
>   , ("stub", Stub.runTest)
>   , ("utf8", UTF8.runTest)
>   , ("util", Util.runTest)
>   ]
