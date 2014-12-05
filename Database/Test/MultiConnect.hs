
|
Module      :  Database.Test.MultiConnect
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Tests Database.Enumerator code in the context of multiple
database connections to different DBMS products.
We should add tests to shift data between databases, too.


> {-# LANGUAGE OverlappingInstances #-}

> module Database.Test.MultiConnect (runTest) where

> import qualified Database.Sqlite.Enumerator as Sqlite
> import qualified Database.PostgreSQL.Enumerator as PG
> import Database.Sqlite.Test.Enumerator as SqlTest
> import Database.PostgreSQL.Test.Enumerator as PGTest
> import Database.Test.Performance as Perf
> import Database.Enumerator
> import System.Environment (getArgs)


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = catchDB ( do
>     let [ user, pswd, dbname ] = args
>     withSession (PG.connect user pswd dbname) $ \sessPG -> do
>     withSession (Sqlite.connect user pswd dbname) $ \sessSql -> do
>       SqlTest.runTest runPerf args
>       PGTest.runTest runPerf args
>   ) basicDBExceptionReporter
