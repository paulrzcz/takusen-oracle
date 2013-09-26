
|
Module      :  Database.Sqlite.Test.SqliteFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


> module Database.Sqlite.Test.SqliteFunctions (runTest) where


> import Foreign
> import Foreign.C
> import Foreign.C.UTF8
> import Control.Monad
> import Control.Exception.Extensible
> import Data.Dynamic
> import Database.Sqlite.SqliteFunctions
> import System.Environment (getArgs)
> import Test.MiniUnit




> runTest :: String -> IO ()
> runTest dbname = do
>   testOpen dbname
>   db <- openDb dbname
>   createFixture db
>   runTestTT "Sqlite low-level tests" (testlist db)
>   destroyFixture db
>   closeDb db


> --testlist db = TestList $ map (\t -> TestCase (t db))
> testlist db = map (\t -> t db)
>   [ testSelectInts
>   , testSelectDouble
>   , testSelectInt64
>   , testSelectStringNull
>   , testSelectStringEmpty
>   , testSelectNoRows
>   , testSelectManyRows
>   , testUnion
>   , testBindString
>   , testBindDouble
>   , testFetchAfterFinalise
>   , testConstraintError
>   , testSelectUTF8Text
>   ]


> ignoreError action =
>   catchSqlite action (\e -> return undefined)

> printIgnoreError action = catchSqlite action 
>     (\e -> do
>       putStrLn (show e)
>       return undefined
>     )

> printPropagateError action = catchSqlite action 
>     (\e -> do
>       putStrLn (show e)
>       throwSqlite e
>       return undefined
>     )

> ddlExec db stmt = do
>   _ <- stmtExec db stmt
>   return ()

> testOpen dbname = do
>   h <- openDb dbname
>   closeDb h

> createFixture db = do
>   ignoreError $ ddlExec db "drop table tdual"
>   ignoreError $ ddlExec db "drop table t_natural"
>   ignoreError $ ddlExec db "drop table t_blob"
>   printPropagateError $
>       ddlExec db "create table tdual (dummy text)"
>   printPropagateError $
>       ddlExec db "insert into tdual (dummy) values ('X')"
>   printPropagateError $ ddlExec db "create table t_natural (n integer primary key)"
>   mapM_ (insertNatural db) [1..10]
>   printPropagateError $
>       ddlExec db "create table t_blob (b blob)"
>   printPropagateError $
>       ddlExec db "insert into t_blob values ('blobtest')"

> insertNatural db n = do
>   ddlExec db $ "insert into t_natural values (" ++ (show n) ++ ")"



> destroyFixture db = do
>   printPropagateError $ ddlExec db "drop table tdual"
>   printPropagateError $ ddlExec db "drop table t_natural"
>   printPropagateError $ ddlExec db "drop table t_blob"


> testCreateDual db = do
>   ignoreError ( do
>       ddlExec db "create table tdual (dummy integer primary key)"
>       assertFailure "SqliteException not thrown when table already exists"
>     )
>   printPropagateError $ ddlExec db "insert into tdual values (1)"




> testSelectInts db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select n from t_natural where n < 3 order by n;"
>   rc <- stmtFetch db stmt
>   n <- colValInt stmt 1
>   assertEqual "testSelectInts: 1" 1 n
>   rc <- stmtFetch db stmt
>   n <- colValInt stmt 1
>   assertEqual "testSelectInts: 2" 2 n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectInts: done" sqliteDONE rc


> testSelectInt64 db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 20041225235959"
>     --stmtPrepare db "select -1 union select 1 union select 2*1000*1000*1000 order by 1"
>   rc <- stmtFetch db stmt
>   n <- colValInt64 stmt 1
>   assertEqual "testSelectInt64: 20041225235959" 20041225235959 n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectInt64: done" sqliteDONE rc


> testSelectDouble db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 1.2"
>   rc <- stmtFetch db stmt
>   n <- colValDouble stmt 1
>   assertEqual "testSelectDouble: 1.2" 1.2 n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectDouble: done" sqliteDONE rc


> testSelectStringNull db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select null"
>   rc <- stmtFetch db stmt
>   n <- colValString stmt 1
>   assertEqual "testSelectString: Nothing" Nothing n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectString: done" sqliteDONE rc


> testSelectStringEmpty db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select ''"
>   rc <- stmtFetch db stmt
>   -- what is rc after row fetch? sqliteOK or sqliteROW?
>   assertEqual "testSelectString: row 1" sqliteROW rc
>   n <- colValString stmt 1
>   assertEqual "testSelectString: ''" (Just "") n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectString: done" sqliteDONE rc


> testUnion db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 'h1' from tdual union select 'h2' from tdual union select 'h3' from tdual"
>   rc <- stmtFetch db stmt
>   Just s <- colValString stmt 1
>   assertEqual "testUnion: h1" "h1" s
>   rc <- stmtFetch db stmt
>   Just s <- colValString stmt 1
>   assertEqual "testUnion: h2" "h2" s
>   rc <- stmtFetch db stmt
>   Just s <- colValString stmt 1
>   assertEqual "testUnion: h3" "h3" s
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testUnion: done" sqliteDONE rc


> testSelectNoRows db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 'h1' from tdual where dummy = '2'"
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectNoRows: done" sqliteDONE rc


> manyRows :: String
> manyRows = 
>   "select 1 from"
>   ++ "  ( select 1 from tdual union select 0 from tdual)"
>   ++ ", ( select 2 from tdual union select 0 from tdual)"
>   ++ ", ( select 3 from tdual union select 0 from tdual)"
>   ++ ", ( select 4 from tdual union select 0 from tdual)"
>   ++ ", ( select 5 from tdual union select 0 from tdual)"
>   ++ ", ( select 6 from tdual union select 0 from tdual)"
>   ++ ", ( select 7 from tdual union select 0 from tdual)"
>   ++ ", ( select 8 from tdual union select 0 from tdual)"
>   ++ ", ( select 9 from tdual union select 0 from tdual)"
>   ++ ", ( select 10 from tdual union select 0 from tdual)"


> countRows :: DBHandle -> StmtHandle -> Int -> IO Int
> countRows db stmt n = do
>   rc <- printPropagateError $ stmtFetch db stmt
>   _ <- colValInt stmt 1
>   if rc == sqliteDONE
>     then return n
>     else countRows db stmt (n+1)

> testSelectManyRows db = do
>   stmt <- printPropagateError $ stmtPrepare db manyRows
>   n <- countRows db stmt 0
>   stmtFinalise db stmt
>   assertEqual "testSelectManyRows: done" 1024 n

> testBindString db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select ? from tdual"
>   bindString db stmt 1 "h1"
>   rc <- stmtFetch db stmt
>   Just n <- colValString stmt 1
>   assertEqual "testBindString: h1" "h1" n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt

> testBindDouble db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select ? from tdual where ? = ?"
>   bindDouble db stmt 1 2.3
>   bindInt db stmt 2 2001
>   bindInt64 db stmt 3 2001
>   rc <- stmtFetch db stmt
>   n <- colValDouble stmt 1
>   assertEqual "testBindDouble: 2.3" 2.3 n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt


This test reveals that library misuse (error number 21)
is reported by sqlite_errcode as error code zero.

> testFetchAfterFinalise db = do
>   stmt <- printPropagateError $ stmtPrepare db manyRows
>   stmtFinalise db stmt
>   rc <- sqliteStep stmt
>   assertEqual "testFetchAfterFinalise: rc" 21 rc
>   ex@(SqliteException e m) <- getError db
>   -- Is this a bug in Sqlite?)
>   assertEqual "testFetchAfterFinalise: errcode" 0 e
>   assertEqual "testFetchAfterFinalise: errmsg" "not an error" m


This test confirms that a constraint error (19)
is correctly reported by sqlite_errcode.

> testConstraintError db = do
>   withUTF8String "insert into t_natural values (1)" $ \cstr -> do
>   rc <- sqliteExec db cstr nullFunPtr nullPtr nullPtr
>   assertEqual "testConstraintError: rc" 19 rc
>   ex@(SqliteException e m) <- getError db
>   assertEqual "testConstraintError: errcode" 19 e
>   assertEqual "testConstraintError: errmsg" "PRIMARY KEY must be unique" m


> testSelectUTF8Text db = do
>   -- GREEK SMALL LETTER PHI
>   -- unicode code-point 966
>   -- UTF8: CF86 (207,134)
>   -- UTF16: 03C6
>   let expect = ['\966']
>   stmt <- printPropagateError $
>     stmtPrepare db "select ?"
>   bindString db stmt 1 expect
>   rc <- stmtFetch db stmt
>   Just n <- colValString stmt 1
>   assertEqual "testSelectUTF8Text" expect n
>   stmtFinalise db stmt
