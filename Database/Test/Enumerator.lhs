
|
Module      :  Database.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Simple test harness. Demonstrates possible usage.

Tests in this module are organised in groups of three/four functions:
  sqlXXX
  iterXXX
  expectXXX
  actionXXX

These have to be tied together by a function which uses backend-specific
functions and types. See the various backend-specific test modules for examples.


> {-# LANGUAGE OverlappingInstances #-}

> module Database.Test.Enumerator where

> import Database.Enumerator
> import Database.Util
> import Data.Time
> import System.Time
> import Data.Int
> import Control.Exception.MonadIO
> import Control.Monad.Trans (liftIO)
> import Test.MiniUnit


> testTable = "takusen_test"

> sqlDropDual = "drop table tdual"
> sqlCreateDual = "create table tdual (dummy varchar(1) primary key)"
> sqlInsertDual = "insert into tdual values ('X')"
> sqlDropTest = "drop table " ++ testTable
> sqlCreateTest = "create table " ++ testTable ++ " (id integer, v varchar(250))"

> sqlInsertTest1 = "insert into " ++ testTable ++ " (id, v) values (1, '2')"
> sqlInsertTest2 = "insert into " ++ testTable ++ " (id, v) values (2, '2')"
> sqlInsertTest3 = "insert into " ++ testTable ++ " (id, v) values (3, '3')"
> sqlInsertTest4 = "insert into " ++ testTable ++ " (id, v) values (4, '4')"


> reportError sql (DBFatal (ssc, sssc) e m) = do
>   putStrLn ("FATAL: " ++ ssc ++ sssc ++ " - " ++ m)
>   putStrLn ("  " ++ sql)
> reportError sql (DBError (ssc, sssc) e m) = do
>   putStrLn ("ERROR: " ++ ssc ++ sssc ++ " - " ++ m)
>   putStrLn ("  " ++ sql)
> reportError sql (DBUnexpectedNull r c) =
>   putStrLn $ "Unexpected null in row " ++ (show r) ++ ", column " ++ (show c) ++ "."
> reportError sql (DBNoData) = putStrLn "Fetch: no more data."

-----------------------------------------------------------

> class DBLiteralValue a where
>   literalDate :: a -> Int64 -> String
>   literalInt :: a -> Int -> String
>   literalInt64 :: a -> Int64 -> String
>   literalFloat :: a -> Float -> String
>   literalDouble :: a -> Double -> String
>   -- default methods
>   literalInt _ i = show i
>   literalInt64 _ i = show i
>   literalFloat _ i = show i
>   literalDouble _ i = show i


> data SqliteFunctions = SqliteFunctions
> data OracleFunctions = OracleFunctions
> data PGSqlFunctions = PGSqlFunctions
> data ODBCFunctions = ODBCFunctions

> instance DBLiteralValue SqliteFunctions where
>   literalDate _ i = dateSqlite i

> instance DBLiteralValue OracleFunctions where
>   literalDate _ i = dateOracle i

> instance DBLiteralValue PGSqlFunctions where
>   literalDate _ i = datePG i
>   literalInt _ i = show i ++ "::int4"
>   literalInt64 _ i = show i ++ "::int8"
>   literalFloat _ i = show i ++ "::float4"
>   literalDouble _ i = show i ++ "::float8"

> instance DBLiteralValue ODBCFunctions where
>   literalDate _ i = dateISO i



> dateSqlite :: Int64 -> String
> dateSqlite i = if i == 0 then "99999999999999" else show i

> dateOracle :: Int64 -> String
> dateOracle i
>   | i == 0 = "to_date(null)"
>   | i > 0  = "to_date('" ++ (zeroPad 14 i) ++ "', 'yyyymmddhh24miss')"
>   | i < 0  = "to_date('" ++ (zeroPad 14 i) ++ "', 'syyyymmddhh24miss')"

> dateISO :: Int64 -> String
> dateISO i =
>   let
>     (year, month, day, hour, minute, second) = int64ToDateParts i
>     zp = zeroPad
>   in
>   if i == 0 then "null"
>   else zp 4 year ++ "-" ++ zp 2 month ++ "-" ++ zp 2 day
>     ++ "T" ++ zp 2 hour ++ ":" ++ zp 2 minute ++ ":" ++ zp 2 second


For postgres:
0000-01-01 AD -> 0001-01-01 BC
0000-12-31 AD -> 0001-12-31 BC
0001-01-01 AD -> 0001-01-01 AD

Using Doubles as storage (not sure about this?)
4714-11-24 BC -> mindate
5874897-12-31 AD -> maxdate
Using 8-bytes ints:
4713 BC -> 294276 AD

Postgres only allows specification of -ve years by use of the AD/BC suffix.
Sadly, this differs from the astronomical year number like so:
astro: ...3,2,1,0,-1,-2,-3,...
ad/bc: ...3AD,2AD,1AD,1BC,2BC,3BC,...

i.e. 0 astro = 1BC, -1 astro = 2BC, etc.

ISO8601 uses astronomical years, so we ought to be able to write
-1000-12-25 (instead of 1001-01-01 BC), but Postgres won't parse this.

So in datePG we want:
int64  0000-01-01 -> 0001-01-01 AD
int64  0001-01-01 -> 0001-01-01 AD
int64 -0001-01-01 -> 0000-01-01 AD
int64 -0002-01-01 -> 0003-01-01 BC
int64 -4712-01-01 -> 4713-01-01 BC

> datePG :: Int64 -> String
> datePG i =
>   let
>     (year1, month, day, hour, minute, second) = int64ToDateParts i
>     year = case () of
>       _ | year1 == 0 -> 1
>         | year1 < 0 -> abs (year1 - 1)
>         | otherwise -> year1
>     suffix = if year1 < 1 then " BC'" else " AD'"
>     zp = zeroPad
>   in
>   if i == 0 then "null::timestamp"
>   --else "timestamp with time zone '" ++ zp 4 year ++ "-" ++ zp 2 month ++ "-" ++ zp 2 day
>   else "timestamp '" ++ zp 4 year ++ "-" ++ zp 2 month ++ "-" ++ zp 2 day
>     ++ " " ++ zp 2 hour ++ ":" ++ zp 2 minute ++ ":" ++ zp 2 second ++ suffix




> execDDL_ s = catchDB (execDDL s) (reportRethrowMsg ("sql: " ++ s ++ "\n"))
> execDML_ s = execDDL_ s

Use execDrop when the DDL is likely to raise an error.
Note that PostgreSQL + ODBC seems to require you commit or rollback the DDL;
if you don't then the next statement will fail with a 25P02
("in failed SQL transaction")
I guess that's a result of PostgreSQL's transactional DDL feature.

> execDrop s = catchDB (withTransaction Serializable (execDDL s)) (\e -> return ())

> makeFixture doDrop doDDL = flip catchDB (reportRethrowMsg "makeFixture: ") $ do
>   doDrop sqlDropDual
>   doDrop sqlDropTest
>   doDDL sqlCreateDual
>   beginTransaction ReadCommitted
>   doDDL sqlInsertDual
>   commit
>   doDDL sqlCreateTest
>   withTransaction Serialisable $ do
>     doDDL sqlInsertTest1
>     doDDL sqlInsertTest2
>     doDDL sqlInsertTest3

> destroyFixture execDDL_ = flip catchDB (reportRethrowMsg "destroyFixture: ") $ do
>   execDDL_ sqlDropDual
>   execDDL_ sqlDropTest
>   catchDB commit (const (return ()))

> selectTest query iter expect = do
>   actual <- doQuery query iter []
>   assertEqual query expect actual



-----------------------------------------------------------

This is used in a few tests...

> sqlSingleValue = "select ? from tdual"

> sqlNoRows = "select dummy from tdual where dummy = 'a' or dummy = '2' "
> iterNoRows (c1::String) acc = result $ c1:acc
> expectNoRows = []::[String]

> sqlTermEarly = "select 'hello1' from tdual union select 'hello2' from tdual union select 'hello3' from tdual order by 1"
> iterTermEarly c1 acc = if c1 == "hello2"
>       then return (Left (c1:acc))
>       else result (c1:acc)
> expectTermEarly = ["hello2", "hello1"]

> sqlFloatsAndInts fns = "select " ++ (literalDouble fns 4841.3403490431) ++ ", "
>   ++ (literalInt fns (-22340234)) ++ " from tdual union select 33311.32332, 23789234 from tdual order by 1"
> --iterFloatsAndInts :: (Monad m) => Double -> Int -> IterAct m [(Double, Int)]
> iterFloatsAndInts (c1::Double) (c2::Int) acc = result $ (c1, c2):acc
> expectFloatsAndInts :: [(Double, Int)]
> expectFloatsAndInts = [ (33311.32332, 23789234) , (4841.3403490431, -22340234) ]

> sqlNullString = "select 'hello1', 'hello2', null from tdual"
> iterNullString :: (Monad m) => String -> String -> Maybe String
>   -> IterAct m [(String, String, Maybe String)]
> iterNullString c1 c2 c3 acc = result $ (c1, c2, c3):acc
> expectNullString = [ ("hello1", "hello2", Nothing) ]

Access can't handle SQL comments (-- or /* */).

> sqlEmptyString = "select 'hello1', 'Oracle always fails this test', '' from tdual"
> iterEmptyString :: (Monad m) => String -> String -> Maybe String
>                          -> IterAct m [(String, String, Maybe String)]
> iterEmptyString c1 c2 c3 acc = result $ (c1, c2, c3):acc
> expectEmptyString = [ ("hello1", "Oracle always fails this test", Just "") ]

> sqlUnhandledNull = "select 'hello1', 'hello2', null from tdual"
> iterUnhandledNull :: (Monad m) => String -> String -> UTCTime
>                          -> IterAct m [(String, String, UTCTime)]
> iterUnhandledNull c1 c2 c3 acc = result $ (c1, c2, c3):acc
> expectUnhandledNull = []

> sqlNullDate fns = "select 'hello1', 'hello2', " ++ (literalDate fns 0) ++ " from tdual"
> iterNullDate :: (Monad m) => String -> String -> Maybe UTCTime
>                          -> IterAct m [(String, String, UTCTime)]
> iterNullDate c1 c2 c3 acc = result $ (c1, c2, ifNull c3 (int64ToUTCTime 10101000000)):acc
> expectNullDate = [ ("hello1", "hello2", (int64ToUTCTime 10101000000)) ]

> sqlDate fns = "select " ++ (literalDate fns 20041224235959) ++ " from tdual"
> iterDate :: (Monad m) => UTCTime -> IterAct m [UTCTime]
> iterDate c1 acc = result $ c1:acc
> expectDate = [ (int64ToUTCTime 20041224235959) ]

> iterCalDate :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
> iterCalDate c1 acc = result $ c1:acc
> expectCalDate = [ (int64ToCalTime 20041224235959) ]


These are the Oracle date boundary cases.

> sqlBoundaryDates fns =
>             "select  " ++ (literalDate fns   99991231000000)  ++ " from tdual"
>   ++ " union select  " ++ (literalDate fns      10101000000)  ++ " from tdual"
>   ++ " union select  " ++ (literalDate fns    (-10101000000)) ++ " from tdual"
>   ++ " union select  " ++ (literalDate fns (-47120101000000)) ++ " from tdual"
>   ++ " order by 1 desc"
> iterBoundaryDates :: (Monad m) => UTCTime -> IterAct m [UTCTime]
> iterBoundaryDates c1 acc = result $ c1:acc
> expectBoundaryDates =
>   [ int64ToUTCTime (-47120101000000)
>   , int64ToUTCTime    (-10101000000)
>   , int64ToUTCTime      10101000000
>   , int64ToUTCTime   99991231000000
>   ]

|Goal: exercise the  "happy path" throught cursor code
i.e. open and fetch all rows, close after last row.

> sqlCursor fns = "select " ++ literalInt fns 1 ++ " from tdual union select " ++ literalInt fns 2 ++ " from tdual"
> iterCursor :: (Monad m) => Int -> IterAct m [Int]
> iterCursor i acc = result $ i:acc
> actionCursor query = do
>   withCursor query iterCursor [] $ \c -> do
>     doneBool <- cursorIsEOF c
>     assertBool query (not doneBool)
>     r <- cursorCurrent c
>     assertEqual query [1] r
>     --
>     cursorNext c
>     doneBool <- cursorIsEOF c
>     assertBool query (not doneBool)
>     r <- cursorCurrent c
>     assertEqual query [2, 1] r
>     --
>     -- Now that the result-set is exhausted, cursorIsEOF returns True,
>     -- and cursorCurrent returns the same value as the previous call to cursorCurrent.
>     cursorNext c
>     doneBool <- cursorIsEOF c
>     assertBool query doneBool
>     r <- cursorCurrent c
>     assertEqual query [2, 1] r
>     --
>     -- What happens if try to advance again?
>     -- We get a DBException: the DBNoData case.
>     --cursorNext c
>     doneBool <- cursorIsEOF c
>     assertBool query doneBool
>     r <- cursorCurrent c
>     assertEqual query [2, 1] r
>     --
>     return ()

|Goal: ensure exception raised when too many rows
fetched from cursor.

This test will raise an exception, as it tries to
fetch too many rows from the cursor.
The exception handler is coded as if we expect the
exception i.e. it ignores it.
The main action should never finish, so there's
a failure assertion at the bottom, just in case
the exception is not raised.

> actionExhaustCursor query = catchDB (
>     withCursor query iterCursor [] $ \c -> do
>       cursorNext c
>       cursorNext c
>       cursorNext c
>       cursorNext c
>       assertFailure "selectExhaustCursor"
>     ) (\e -> return () )

> sqlBindString = "select ? from tdual union select ? from tdual order by 1"
> iterBindString :: (Monad m) => String -> IterAct m [String]
> iterBindString i acc = result $ i:acc
> expectBindString = ["b1", "a2"]
> actionBindString stmt bindVals = do
>   withTransaction Serialisable $ do
>   withPreparedStatement stmt $ \pstmt -> do
>   withBoundStatement pstmt bindVals $ \bstmt -> do
>     actual <- doQuery bstmt iterBindString []
>     assertEqual sqlBindString expectBindString actual


Each back-end has it's own idea of parameter placeholder syntax.
We currently support ?-style as a lowest-common-denominator,
and each back-end converts occurences of "?"
to the back-end-specific style where required.
If you use a back-end specific style, you can expect it to be passed
through unmolested.
  ?  : ODBC, MS Sql Server, Sqlite
  :n : Oracle
  $n : Postgres

> sqlBindInt = "select ? from tdual union select ? from tdual order by 1"
> iterBindInt :: (Monad m) => Int -> IterAct m [Int]
> iterBindInt i acc = result $ i:acc
> expectBindInt :: [Int]; expectBindInt = [2, 1]
> actionBindInt stmt bindVals = do
>   withTransaction Serialisable $ do
>   withPreparedStatement stmt $ \pstmt -> do
>   withBoundStatement pstmt bindVals $ \bstmt -> do
>     actual <- doQuery bstmt iterBindInt []
>     assertEqual sqlBindInt expectBindInt actual

> sqlBindIntDoubleString = "select ?,?,? from tdual union select ?,?,? from tdual order by 1"
> iterBindIntDoubleString :: (Monad m) => Int -> Double -> String -> IterAct m [(Int, Double, String)]
> iterBindIntDoubleString i d s acc = result $ (i, d, s):acc
> expectBindIntDoubleString :: [(Int, Double, String)]
> expectBindIntDoubleString = [(3, 4.4, "row 2"), (1, 2.2, "row 1")]
> actionBindIntDoubleString stmt = do
>   withTransaction Serialisable $ do
>     actual <- doQuery stmt iterBindIntDoubleString []
>     assertEqual sqlBindIntDoubleString expectBindIntDoubleString actual

> sqlBindDate = sqlSingleValue
> iterBindDate :: (Monad m) => UTCTime -> IterAct m [UTCTime]
> iterBindDate c1 acc = result $ c1:acc
> expectBindDate = [ int64ToUTCTime 20041224235959 ]
> actionBindDate stmt = do
>   withTransaction Serialisable $ do
>     actual <- doQuery stmt iterBindDate []
>     assertEqual sqlBindDate expectBindDate actual

> sqlBindBool = "select ?, ? from tdual"
> iterBindBool :: (Monad m) => Bool -> Bool -> IterAct m [(Bool, Bool)]
> iterBindBool b1 b2 acc = result $ (b1,b2):acc
> expectBindBool = [ (True, False) ]
> actionBindBool stmt = do
>   withTransaction Serialisable $ do
>     actual <- doQuery stmt iterBindBool []
>     assertEqual sqlBindBool expectBindBool actual

> sqlBindBoundaryDates =
>             "select ? from tdual"
>   ++ " union select ? from tdual"
>   ++ " union select ? from tdual"
>   ++ " union select ? from tdual"
>   ++ " order by 1 desc"
> actionBindBoundaryDates stmt = do
>   withTransaction Serialisable $ do
>     actual <- doQuery stmt iterBindDate []
>     assertEqual sqlBindBoundaryDates expectBoundaryDates actual


> sqlBoundStmtDML = "insert into " ++ testTable ++ " (id, v) values (?, ?)"
> actionBoundStmtDML stmt = do
>   withPreparedStatement stmt $ \pstmt -> do
>     -- do it twice, to check that prepared stmt can be reused.
>     -- Note that with bound statements, the query (or command)
>     -- already been executed, so it's too late to begin the transaction.
>     beginTransaction Serialisable
>     withBoundStatement pstmt [bindP (100::Int), bindP "100"] $ \bstmt -> do
>       count <- execDML bstmt
>       rollback
>       assertEqual sqlBoundStmtDML 1 count
>     beginTransaction Serialisable
>     withBoundStatement pstmt [bindP (100::Int), bindP "100"] $ \bstmt -> do
>       count <- execDML bstmt
>       rollback
>       assertEqual sqlBoundStmtDML 1 count



With 'MyTree' we test the ability to send and receive arbtrary Show-able
values as Strings i.e. we create our own datatype for the test.

> data MyTree a = Leaf a | Branch [MyTree a] deriving (Eq, Show, Read)

> sqlPolymorphicFetch = sqlSingleValue
> iterPolymorphicFetch :: (Monad m) => MyTree String -> IterAct m (MyTree String)
> iterPolymorphicFetch v _ = result' v
> expectPolymorphicFetch = Branch [Leaf "a", Leaf "b", Branch [Leaf "c", Leaf "d"], Leaf "e"]
> actionPolymorphicFetch stmt = do
>   actual <- doQuery stmt iterPolymorphicFetch (Leaf "")
>   assertEqual sqlPolymorphicFetch expectPolymorphicFetch actual

> sqlPolymorphicFetchNull = "select '' from tdual"
> iterPolymorphicFetchNull :: (Monad m) => Maybe (MyTree String) -> IterAct m (Maybe (MyTree String))
> iterPolymorphicFetchNull v _ = result' v
> expectPolymorphicFetchNull :: Maybe (MyTree String)
> expectPolymorphicFetchNull = Nothing
> actionPolymorphicFetchNull stmt = do
>   actual <- doQuery stmt iterPolymorphicFetchNull Nothing
>   assertEqual sqlPolymorphicFetchNull Nothing actual

> sqlRebind = sqlSingleValue
> iterRebind (i::Int) acc = result $ i:acc
> expectRebind1 :: [Int]; expectRebind1 = [1]
> expectRebind2 :: [Int]; expectRebind2 = [2]
> actionRebind stmt bindVal1 bindVal2 = do
>   withPreparedStatement stmt $ \pstmt -> do
>     withBoundStatement pstmt bindVal1 $ \bstmt -> do
>       actual <- doQuery bstmt iterRebind []
>       assertEqual sqlRebind expectRebind1 actual
>     withBoundStatement pstmt bindVal2 $ \bstmt -> do
>       actual <- doQuery bstmt iterRebind []
>       assertEqual sqlRebind expectRebind2 actual

> sqlExceptionRollback = "select count(*) from " ++ testTable
> iterExceptionRollback (i::Int) acc = result $ i:acc
> actionExceptionRollback insertStmt selectStmt = do
>   catchDB (
>     withTransaction Serialisable $ do
>       execDML insertStmt
>       throwDB DBNoData
>     ) (\e -> return () )
>   count <- doQuery selectStmt iterExceptionRollback []
>   assertEqual sqlExceptionRollback [3] count

> sqlIterateeMatchesResultSet = "select ?,?,? from tdual"
> --iterMatchesResultSet :: (Monad m) => Int -> Double -> String -> IterAct m [(Int, Double, String)]
> --iterMatchesResultSet i d s acc = result $ (i, d, s):acc
> iterMatchesResultSet :: (Monad m) => Int -> Double -> String -> String -> IterAct m [(Int, Double, String, String)]
> iterMatchesResultSet i d s s2 acc = result $ (i, d, s, s2):acc
> actionIterateeMatchesResultSet stmt = do
>   catchDB ( do
>       actual <- doQuery stmt iterMatchesResultSet []
>       assertFailure "actionIterateeMatchesResultSet"
>     ) (\e -> return () )
