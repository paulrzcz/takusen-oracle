
|
Module      :  Database.ODBC.Test.OdbcFunctions
Copyright   :  (c) 2007 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


-- FIXME  add tests for moreResults

> module Database.ODBC.Test.OdbcFunctions where

> import Database.ODBC.OdbcFunctions
> import Control.Exception.Extensible (finally)
> import Control.Monad (liftM, when)
> import Data.Char
> import Data.List
> import Data.Time
> import Data.Word (Word8)
> import Database.Util
> import Test.MiniUnit
> import Foreign.ForeignPtr (withForeignPtr)
> import Foreign.Ptr (castPtr)
> import Foreign.Storable (peek)
> import Foreign.Marshal.Array (peekArray0, peekArray)
> import Numeric (showHex)


> ignoreError action =
>   catchOdbc action (\e -> return undefined)

> printIgnoreError action = catchOdbc action 
>     (\e -> do
>       putStrLn (show e)
>       return undefined
>     )

> printPropagateError action = catchOdbc action 
>     (\e -> do
>       putStrLn (show e)
>       throwOdbc e
>       return undefined
>     )

> testCreateEnv = do
>   env <- allocEnv
>   freeEnv env

> testCreateConn = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   freeConn conn
>   freeEnv env

> testConnect connstr = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   connstr <- connect conn connstr
>   disconnect conn
>   freeConn conn
>   freeEnv env

> execSql conn sql = do
>   stmt <- allocStmt conn
>   prepareStmt stmt sql
>   executeStmt stmt
>   freeStmt stmt

> createConn connstr = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   connstr <- connect conn connstr
>   setConnEncoding conn EncUTF8
>   dbmsname <- liftM (map toLower) (getInfoDbmsName conn)
>   when (dbmsname == "postgresql") (execSql conn "set client_encoding = 'UTF8'")
>   return (env, conn { connDbms = map toLower dbmsname } )

> closeConn (env, conn) = do
>   disconnect conn
>   freeConn conn
>   freeEnv env

> createDual conn = do
>   execSql conn "create table tdual (dummy varchar(1) primary key)"
>   execSql conn "insert into tdual values ('X')"
>   commit conn

> dropDual conn = execSql conn "drop table tdual"

> testCreateStmt conn = execSql conn "select 'x' from tdual"


> testIsolationLevelReadUncommitted conn = setTxnIsolation conn sqlTxnReadUncommitted
> testIsolationLevelReadCommitted conn = setTxnIsolation conn sqlTxnReadCommitted
> testIsolationLevelRepeatableRead conn = setTxnIsolation conn sqlTxnRepeatableRead
> testIsolationLevelSerializable conn = setTxnIsolation conn sqlTxnSerializable

Uses getData, rather than a buffer.

> testFetchString conn = do
>   stmt <- allocStmt conn
>   -- Oracle doesn't like codepoints > 0x10FF40; perhaps this is special in Unicode?
>   let string1 = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FF40]
>   let string2 = "xyz" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FF40]
>   prepareStmt stmt ("select '" ++ string1
>     ++ "' from tdual union select '" ++ string2 ++ "' from tdual order by 1")
>   executeStmt stmt
>   more <- fetch stmt
>   s <- getDataString stmt 1
>   assertEqual "testFetchString" (Just string1) s
>   more <- fetch stmt
>   s <- getDataString stmt 1
>   assertEqual "testFetchString" (Just string2) s
>   more <- fetch stmt
>   assertBool "testFetchString: EOD" (not more)
>   freeStmt stmt

> testFetchStringWithBuffer conn = do
>   stmt <- allocStmt conn
>   -- Oracle doesn't like codepoints > 0x10FF40; perhaps this is special in Unicode?
>   let string1 = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FF40]
>   let string2 = "xyz" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FF40]
>   prepareStmt stmt ("select '" ++ string1
>     ++ "' from tdual union select '" ++ string2 ++ "' from tdual order by 1")
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getStringFromBuffer buffer
>   assertEqual "testFetchStringWithBuffer" (Just string1) s
>   more <- fetch stmt
>   s <- getStringFromBuffer buffer
>   assertEqual "testFetchStringWithBuffer" (Just string2) s
>   more <- fetch stmt
>   assertBool "testFetchStringWithBuffer: EOD" (not more)
>   freeStmt stmt

> testFetchInt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 101 from tdual"
>   executeStmt stmt
>   more <- fetch stmt
>   putStrLn "testFetchInt: call getData"
>   s <- getData stmt 1
>   let expect :: Int; expect = 101
>   putStrLn "testFetchInt: assert"
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testFetchIntWithBuffer conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 101 from tdual"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 0 (Just (0::Int))
>   more <- fetch stmt
>   s <- getFromBuffer buffer
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testFetchDouble conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 123.456789 from tdual"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 0 (Just (0::Double))
>   more <- fetch stmt
>   s <- getFromBuffer buffer
>   --s <- getData stmt 1
>   let expect :: Double; expect = 123.456789
>   assertEqual "testFetchDouble" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchDouble: EOD" (not more)
>   freeStmt stmt

> testFetchDatetime conn = do
>   stmt <- allocStmt conn
>   dbmsname <- liftM (map toLower) (getInfoDbmsName conn)
>   --putStrLn ("testFetchDatetime: dbmsname = " ++ dbmsname)
>   flip finally (freeStmt stmt) ( do
>     -- There is no common SQL standard for datetime literal text.
>     -- Well, there is (timestamp), but MS SQL Server doesn't support it. Pah.
>     -- And Oracle seems to also require a sane NLS setting e.g.
>     --   alter session set NLS_TIMESTAMP_FORMAT = 'yyyy-mm-dd hh24:mi:ss'
>     when (dbmsname == "oracle") ( do
>         prepareStmt stmt "alter session set NLS_TIMESTAMP_FORMAT = 'yyyy-mm-dd hh24:mi:ss'"
>         executeStmt stmt
>       )
>     --
>     -- PostgreSQL doesn't appear to support the {fn ...} escape sequences;
>     -- the text seems to be passed through unchanged (according to SQLNativeSql, anyway).
>     --getNativeSql conn "select {fn CONVERT('1916-10-01 02:25:21', SQL_TIMESTAMP)}, {fn CONVERT('2005-10-01 00:00:00', SQL_TIMESTAMP)} from tdual"
>     --
>     -- THe MS SQL Server ODBC driver's SQLNativeSql also returns the text unchanged,
>     -- but it doesn't seem to cause any grief on the server.
>     --getNativeSql conn "select {fn CONVERT('1916-10-01 02:25:21', SQL_DATETIME)}, {fn CONVERT('2005-10-01 00:00:00', SQL_DATETIME)}"
>     --
>     -- Access doesn't seem to like the {fn ...} escape sequences either,
>     -- so we have to use VBA function CDate to convert text to datetime.
>     --getNativeSql conn "select cdate('1916-10-01 02:25:21'), cdate('2005-10-01 00:00:00')"
>     --  >>= putStrLn
>     --getNativeSql conn "select {fn convert('1916-10-01 02:25:21', SQL_DATETIME)}"
>     --  >>= putStrLn
>     case dbmsname of
>       "postgresql" -> 
>         prepareStmt stmt "select cast ('1916-10-01 02:25:21' as timestamp), cast ('2005-10-01 00:00:00' as timestamp)"
>       -- MS Access doesn't like SQL_TIMESTAMP datatype, but accepts SQL_DATETIME.
>       "access" ->
>         prepareStmt stmt "select cdate('1916-10-01 02:25:21'), cdate('2005-10-01 00:00:00')"
>         --prepareStmt stmt "select {fn CONVERT('1916-10-01 02:25:21', SQL_DATETIME)}, {fn CONVERT('2005-10-01 00:00:00', SQL_DATETIME)}"
>       otherwise ->
>         prepareStmt stmt "select {fn CONVERT('1916-10-01 02:25:21', SQL_TIMESTAMP)}, {fn CONVERT('2005-10-01 00:00:00', SQL_TIMESTAMP)} from tdual"
>     executeStmt stmt
>     let expect1 = mkUTCTime 1916 10  1  2 25 21
>     let expect2 = mkUTCTime 2005 10  1  0  0  0
>     buffer1 <- bindColBuffer stmt 1 0 (Just expect1)
>     buffer2 <- bindColBuffer stmt 2 0 (Just expect2)
>     more <- fetch stmt
>     t1 <- getFromBuffer buffer1
>     t2 <- getFromBuffer buffer2
>     assertEqual "testFetchDatetime1" (Just expect1) t1
>     assertEqual "testFetchDatetime2" (Just expect2) t2
>     more <- fetch stmt
>     assertBool "testFetchDatetime: EOD" (not more)
>     )


> testBindInt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   bindParamBuffer stmt 1 (Just (101::Int)) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 0 (Just (0::Int))
>   more <- fetch stmt
>   s <- getFromBuffer buffer
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testBindDouble conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   -- null value first
>   bindParamBuffer stmt 1 (Nothing :: Maybe Double) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 0 (Just (0::Double))
>   more <- fetch stmt
>   s <- getFromBuffer buffer
>   assertEqual "testBindDouble" (Nothing :: Maybe Double) s
>   -- non-null value
>   closeCursor stmt
>   bindParamBuffer stmt 1 (Just 101.101 :: Maybe Double) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 0 (Just (0::Double))
>   more <- fetch stmt
>   s <- getFromBuffer buffer
>   let d :: Double; d = 101.101;
>   assertEqual "testBindDouble" (Just d) s
>   --
>   more <- fetch stmt
>   assertBool "testBindDouble: EOD" (not more)
>   --
>   freeStmt stmt

> testBindString conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   let expect = "abcdefghijklmnopqrstuvwxyz"
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 1000 (Just expect)
>   more <- fetch stmt
>   s <- getStringFromBuffer buffer
>   assertEqual "testBindString" (Just expect) s
>   more <- fetch stmt
>   assertBool "testBindString: EOD" (not more)
>   -- test closeCursor can be called if we want to
>   closeCursor stmt  -- not necessary when done just before freeStmt
>   freeStmt stmt

> testBindStringUTF8 conn = do
>   stmt <- allocStmt conn
>   setStmtEncoding stmt EncUTF8  -- just in case UTF8 is not default
>   prepareStmt stmt "select ? from tdual"
>   -- Oracle doesn't like codepoints > 0x10FF40; perhaps this is special in Unicode?
>   --let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FF40]
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 1000 (Just expect)
>   more <- fetch stmt
>   s <- getStringFromBuffer buffer
>   assertEqual "testBindStringUTF8 (PostgreSQL fails with Unicode driver, succeeds with ANSI)" (Just expect) s
>   more <- fetch stmt
>   assertBool "testBindStringUTF8: EOD" (not more)
>   freeStmt stmt

> testBindUTCTime conn = do
>   stmt <- allocStmt conn
>   flip finally (freeStmt stmt) ( do
>     prepareStmt stmt "select ? from tdual"
>     let expect :: UTCTime; expect = mkUTCTime 1971 10  1  2 25 21
>     bindbuf <- bindParamBuffer stmt 1 (Just expect) 0
>     executeStmt stmt
>     buffer <- bindColBuffer stmt 1 undefined (Just expect)
>     more <- fetch stmt
>     t <- getUtcTimeFromBuffer buffer
>     assertEqual "testBindUTCTime" (Just expect) t
>     more <- fetch stmt
>     assertBool "testBindUTCTime: EOD" (not more)
>     )


> testBindUTCTimeBoundary conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ?, ? from tdual"
>   -- 1753 seems to be about the earliest year MS SQL Server supports.
>   let expect1 = mkUTCTime 1753 1 1 0 0 0
>   let expect2 = mkUTCTime 9999 10  1  2 25 21
>   let input1 = expect1
>   let input2 = expect2
>   inbuf1 <- bindParamBuffer stmt 1 (Just input1) 0
>   inbuf2 <- bindParamBuffer stmt 2 (Just input2) 0
>   executeStmt stmt
>   buffer1 <- bindColBuffer stmt 1 100 (Just expect1)
>   buffer2 <- bindColBuffer stmt 2 100 (Just expect2)
>   more <- fetch stmt
>   t1 <- getFromBuffer buffer1
>   t2 <- getFromBuffer buffer2
>   assertEqual "testBindUTCTimeBoundary1" (Just expect1) t1
>   assertEqual "testBindUTCTimeBoundary2" (Just expect2) t2
>   more <- fetch stmt
>   assertBool "testBindUTCTimeBoundary: EOD" (not more)
>   freeStmt stmt

> testRebind conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   --
>   --closeCursor stmt  -- can't do this - function sequence error
>   --
>   let expect = "abc"
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getStringFromBuffer buffer
>   assertEqual "testRebind1" (Just expect) s
>   more <- fetch stmt
>   assertBool "testRebind1: EOD" (not more)
>   --
>   closeCursor stmt  -- reset stmt for rebinding, but do not dealloc
>   --
>   let expect = "xyz"
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getStringFromBuffer buffer
>   assertEqual "testRebind2" (Just expect) s
>   more <- fetch stmt
>   assertBool "testRebind2: EOD" (not more)
>   --
>   closeCursor stmt
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   closeCursor stmt
>   --
>   freeStmt stmt


> testUTF8 conn = do
>   stmt <- allocStmt conn
>   setStmtEncoding stmt EncUTF8  -- just in case UTF8 is not default
>   prepareStmt stmt "drop table t_utf8"
>   ignoreError (executeStmt stmt)
>   prepareStmt stmt "create table t_utf8(s varchar(50))"
>   executeStmt stmt
>   --let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   -- Oracle doesn't like codepoints > 0x10FF40; perhaps this is special in Unicode?
>   let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FF40]
>   prepareStmt stmt ("insert into t_utf8 values ( '" ++ expect ++ "' )")
>   executeStmt stmt
>   prepareStmt stmt "select s from t_utf8"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just expect)
>   more <- fetch stmt
>   s <- getStringFromBuffer buffer
>   assertEqual "testUTF8" (Just expect) s
>   freeStmt stmt

> testDbmsName conn = do
>   (liftM ("dbms-name: " ++) (getInfoDbmsName conn)) >>= putStrLn
>   (liftM ("dbms-ver: " ++) (getInfoDbmsVer conn)) >>= putStrLn
>   (liftM ("db-name: " ++) (getInfoDatabaseName conn)) >>= putStrLn
>   (liftM ("driver-name: " ++) (getInfoDriverName conn)) >>= putStrLn
>   (liftM ("driver-ver: " ++) (getInfoDriverVer conn)) >>= putStrLn


> dropFixtureMultiResultSet = "DROP PROCEDURE takusenTestProc"
> makeFixtureMultiResultSet = "CREATE PROCEDURE takusenTestProc"
>   ++ " AS BEGIN"
>   ++ " SELECT '1', '2' \n\n"
>   ++ " SELECT '3', '4' \n\n"
>   ++ " END;"

> testMultiResultSet conn = do
>   name <- liftM (map toLower) (getInfoDbmsName conn)
>   when (name == "microsoft sql server") $ do
>   stmt <- allocStmt conn
>   printIgnoreError (prepareStmt stmt dropFixtureMultiResultSet)
>   ignoreError (executeStmt stmt)
>   printIgnoreError (prepareStmt stmt makeFixtureMultiResultSet)
>   printIgnoreError (executeStmt stmt)
>   prepareStmt stmt "{call takusenTestProc}"
>   executeStmt stmt
>   --
>   buffer1 <- bindColBuffer stmt 1 100 (Just "")
>   buffer2 <- bindColBuffer stmt 2 100 (Just "")
>   more <- fetch stmt
>   s1 <- getStringFromBuffer buffer1
>   assertEqual "testMultiResultSet1" (Just "1") s1
>   s2 <- getStringFromBuffer buffer2
>   assertEqual "testMultiResultSet2" (Just "2") s2
>   more <- fetch stmt
>   assertBool "testMultiResultSet1: EOD" (not more)
>   --
>   more <- moreResults stmt
>   assertBool "testMultiResultSet: more result-sets" more
>   --
>   buffer1 <- bindColBuffer stmt 1 100 (Just "")
>   buffer2 <- bindColBuffer stmt 2 100 (Just "")
>   more <- fetch stmt
>   s1 <- getStringFromBuffer buffer1
>   assertEqual "testMultiResultSet3" (Just "3") s1
>   s2 <- getStringFromBuffer buffer2
>   assertEqual "testMultiResultSet4" (Just "4") s2
>   more <- fetch stmt
>   assertBool "testMultiResultSet3: EOD" (not more)
>   --
>   more <- moreResults stmt
>   assertBool "testMultiResultSet: no more result-sets" (not more)
>   --
>   prepareStmt stmt dropFixtureMultiResultSet
>   executeStmt stmt
>   freeStmt stmt


> dropFixtureBindOutput = "DROP PROCEDURE takusenTestProc"
> makeFixtureBindOutputSqlServer = "CREATE PROCEDURE takusenTestProc @x int output, @y varchar(200) output"
>   ++ " AS BEGIN"
>   ++ " declare @z varchar(200) \n\n"
>   ++ " select @x = 2 * @x \n\n"
>   ++ " select @z = 'output ' + @y + ' xxx' \n\n"
>   ++ " select @y = @z \n\n"
>   ++ " END;"

> makeFixtureBindOutputOracle = "CREATE or replace PROCEDURE takusenTestProc(x in out number, y in out varchar)"
>   ++ " AS BEGIN"
>   ++ " x := x * 2;"
>   ++ " y := 'output ' || y || ' xxx';"
>   ++ " END;"

> testBindOutput conn = do
>   dbmsname <- liftM (map toLower) (getInfoDbmsName conn)
>   case dbmsname of
>     "microsoft sql server" -> k makeFixtureBindOutputSqlServer
>     "oracle" -> k makeFixtureBindOutputOracle
>     _ -> return ()
>   where
>   k makeFixture = do
>   stmt <- allocStmt conn
>   prepareStmt stmt dropFixtureBindOutput
>   ignoreError (executeStmt stmt)
>   prepareStmt stmt makeFixture
>   executeStmt stmt
>   --
>   prepareStmt stmt "{call takusenTestProc(?,?)}"
>   let input1 :: Int; input1 = 1234
>   let input2 = "message"
>   buffer1 <- bindParamBuffer stmt 1 (InOutParam (Just input1)) 0
>   -- max size for string out parameter is 8000 - SQL Server errors on anything >8000.
>   -- bindParamString adds 1 to the size, so we can pass 7999 max.
>   buffer2 <- bindParamBuffer stmt 2 (InOutParam (Just input2)) 7999
>   executeStmt stmt
>   --
>   i <- getFromBuffer buffer1
>   assertEqual "testBindOutput" (Just (2*input1)) i
>   s <- getStringFromBuffer buffer2
>   assertEqual "testBindOutput" (Just "output message xxx") s
>   --more <- fetch stmt  -- This will cause error:
>   --
>   --more <- moreResults stmt
>   --assertBool "testBindOutput: more result-sets" (not more)
>   --
>   prepareStmt stmt dropFixtureBindOutput
>   executeStmt stmt
>   freeStmt stmt



> printBufferContents buffer = do
>   withForeignPtr (bindBufPtr buffer) $ \bptr -> do
>   withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
>   sz <- peek szptr
>   printArrayContents (fromIntegral sz) (castPtr bptr)


> testlist =
>   testCreateStmt :
>   testIsolationLevelReadUncommitted :
>   testIsolationLevelReadCommitted :
>   testIsolationLevelRepeatableRead :
>   testIsolationLevelSerializable :
>   testFetchString :
>   testFetchInt :
>   testFetchStringWithBuffer :
>   testFetchIntWithBuffer :
>   testFetchDouble :
>   testFetchDatetime :
>   testBindInt :
>   testBindDouble :
>   testBindString :
>   testBindStringUTF8 :
>   testBindUTCTime :
>   testBindUTCTimeBoundary :
>   testUTF8 :
>   testRebind :
>   testMultiResultSet :
>   testBindOutput :
>   --testDbmsName :
>   []


> mkTestlist conn testlist = map (\testcase -> printPropagateError (testcase conn)) testlist

> parseArgs :: [String] -> IO String
> parseArgs args = do
>    let (dsn:_) = args
>    return dsn

> runTest :: [String] -> IO ()
> runTest as = do
>   connstr <- parseArgs as
>   printPropagateError testCreateEnv
>   printPropagateError testCreateConn
>   printPropagateError (testConnect connstr)
>   (env, conn) <- printPropagateError (createConn connstr)
>   ignoreError (dropDual conn)
>   printPropagateError (createDual conn)
>   counts <- runTestTT "ODBC low-level tests" (mkTestlist conn testlist)
>   printPropagateError (dropDual conn)
>   closeConn (env, conn)
>   return ()
