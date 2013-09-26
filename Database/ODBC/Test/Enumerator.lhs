
|
Module      :  Database.ODBC.Test.Enumerator
Copyright   :  (c) 2007 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


> module Database.ODBC.Test.Enumerator (runTest) where

> import qualified Database.ODBC.Test.OdbcFunctions as Low
> import Database.ODBC.Enumerator
> import Database.Test.Performance as Perf
> import Database.Test.Enumerator
> import Database.Util
> import Control.Monad (liftM, when)
> import Control.Monad.Trans (liftIO)
> import Data.Char
> import Data.Time
> import Test.MiniUnit


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   let (dsn:_) = args
>   Low.runTest args
>   flip catchDB basicDBExceptionReporter $ do
>     (r, conn1) <- withContinuedSession (connect dsn) (testBody runPerf)
>     withSession conn1 testPartTwo

> testBody :: Perf.ShouldRunTests -> DBM mark Session ()
> testBody runPerf = do
>   runFixture ODBCFunctions
>   when (runPerf == Perf.RunTests) runPerformanceTests

> testPartTwo :: DBM mark Session ()
> testPartTwo = do
>   makeFixture execDrop execDDL_
>   destroyFixture execDDL_

> runFixture :: DBLiteralValue a => a -> DBM mark Session ()
> runFixture fns = do
>   setStringEnc EncUTF8
>   --dbmsname <- inquire InfoDbmsName
>   --when (dbmsname == "postgres") (execDDL_ "set client_encoding = 'UTF8'")
>   makeFixture execDrop execDDL_
>   runTestTT "ODBC tests" (map (runOneTest fns) testList)
>   destroyFixture execDDL_

> runOneTest fns t = catchDB (t fns) (reportRethrowMsg "runOneTest ")

> runPerformanceTests :: DBM mark Session ()
> runPerformanceTests = do
>   makeFixture execDrop execDDL_
>   beginTransaction ReadCommitted
>   runTestTT "ODBC performance tests" (map (flip catchDB reportRethrow)
>     -- The PostgreSQL ODBC driver has genetic query optimisation disabled
>     -- by default. This makes it really struggle with the query we use
>     -- for performance testing - the memory used by the server process
>     -- goes through the roof.
>     [ timedSelect (prefetch 1 sqlRows2Power17 []) 4 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power17 []) 4 (2^17)
>     , timedCursor (prefetch 1 sqlRows2Power17 []) 4 (2^17)
>     , timedCursor (prefetch 1000 sqlRows2Power17 []) 4 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power20 []) 35 (2^20)
>     ]
>     )
>   commit
>   destroyFixture execDDL_


-----------------------------------------------------------

> selectNoRows _ = selectTest sqlNoRows iterNoRows expectNoRows

> selectTerminatesEarly _ = selectTest sqlTermEarly iterTermEarly expectTermEarly

> selectFloatsAndInts fns = selectTest (sqlFloatsAndInts fns) iterFloatsAndInts expectFloatsAndInts

> selectNullString _ = selectTest sqlNullString iterNullString expectNullString

> selectEmptyString _ = selectTest sqlEmptyString iterEmptyString expectEmptyString

> selectUnhandledNull _ = catchDB ( do
>       selectTest sqlUnhandledNull iterUnhandledNull expectUnhandledNull
>       assertFailure sqlUnhandledNull
>   ) (\e -> return () )



> selectNullDate dateFn = selectTest (sqlNullDate dateFn) iterNullDate expectNullDate

> selectCursor fns = actionCursor (sqlCursor fns)
> selectExhaustCursor fns = actionExhaustCursor (sqlCursor fns)

> selectBindString _ = actionBindString
>     (prepareQuery (sql sqlBindString))
>     [bindP "a2", bindP "b1"]

> selectBindInt _ = actionBindInt
>   (prepareQuery (sql sqlBindInt))
>   [bindP (1::Int), bindP (2::Int)]


> selectBindIntDoubleString _ = actionBindIntDoubleString
>   (prefetch 1 sqlBindIntDoubleString [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int), bindP (4.4::Double), bindP "row 2"])

> selectBindDate _ = actionBindDate
>   (prefetch 1 sqlBindDate (map bindP expectBindDate))

Access doesn't seem to like bind variables with union; get:
  22018 39: [Microsoft][ODBC Microsoft Access Driver]Invalid character value for cast specification on column number 1
So we have our own local variation on the boundary dates test,
where we select one row with three columns, rather than three rows with one column.

> sqlBindBoundaryDatesLocal = "select ?, ?, ? from tdual"
> iterBindBoundaryDatesLocal :: (Monad m) => UTCTime -> UTCTime -> UTCTime -> IterAct m [(UTCTime, UTCTime, UTCTime)]
> iterBindBoundaryDatesLocal d1 d2 d3 acc = result $ (d1,d2,d3):acc
> -- 1753 seems to be about the earliest year MS SQL Server supports.
> expectBoundaryDatesLocal = [(int64ToUTCTime 17530101000000, int64ToUTCTime 19990102000000, int64ToUTCTime 99991231000000)]
> actionBindBoundaryDatesLocal stmt = do
>   withTransaction Serialisable $ do
>     actual <- doQuery stmt iterBindBoundaryDatesLocal []
>     assertEqual sqlBindBoundaryDatesLocal expectBoundaryDatesLocal actual
> selectBindBoundaryDates _ = actionBindBoundaryDatesLocal
>   (prefetch 1 sqlBindBoundaryDatesLocal
>     [ bindP (int64ToUTCTime 17530101000000)
>     , bindP (int64ToUTCTime 19990102000000)
>     , bindP (int64ToUTCTime 99991231000000)
>     ])


> selectRebindStmt _ = actionRebind (prepareQuery (sql sqlRebind))
>    [bindP (1::Int)] [bindP (2::Int)]

> boundStmtDML _ = actionBoundStmtDML (prepareCommand (sql sqlBoundStmtDML))
> boundStmtDML2 _ = do
>   -- With MS SQL Server cannot use withTransaction with rollback/commit;
>   -- if you explicitly end the transaction, then when withTransaction
>   -- attempts to end it (with a commit, in the success case) then we
>   -- get a "logic error".
>   -- This differs from PostgreSQL and Oracle, who don't seem to care if
>   -- you commit or rollback too many times.
>   beginTransaction ReadCommitted
>   count <- execDML (cmdbind sqlBoundStmtDML [bindP (100::Int), bindP "100"])
>   rollback
>   assertEqual sqlBoundStmtDML 1 count

> polymorphicFetchTest _ = actionPolymorphicFetch
>   (prefetch 1 sqlPolymorphicFetch [bindP expectPolymorphicFetch])

> polymorphicFetchTestNull _ = actionPolymorphicFetchNull
>   (prefetch 1 sqlPolymorphicFetchNull [])

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4 sqlExceptionRollback

> iterateeMatchesResultSet _ = actionIterateeMatchesResultSet
>   (prefetch 0 sqlIterateeMatchesResultSet [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int)])


> dropFixtureMultiResultSet1 = "DROP VIEW t_whole"
> makeFixtureMultiResultSet1 = "CREATE VIEW t_whole as"
>   ++ " select 0 as n  union select 1"
>   ++ " union select 2 union select 3"
>   ++ " union select 4 union select 5"
>   ++ " union select 6 union select 7"
>   ++ " union select 8 union select 9"

> dropFixtureMultiResultSet2 = "DROP VIEW t_natural"
> makeFixtureMultiResultSet2 = "CREATE VIEW t_natural as"
>   ++ " select n from"
>   ++ " ( select t1.n + 10 * t10.n + 100 * t100.n as n"
>   ++ "   from t_whole t1, t_whole t10, t_whole t100"
>   ++ " ) t where n > 0"

> dropFixtureMultiResultSet3 = "DROP PROCEDURE takusenTestProc"
> makeFixtureMultiResultSet3 = "CREATE PROCEDURE takusenTestProc"
>   ++ " AS BEGIN"
>   ++ " SELECT n*n from t_natural where n < 10 order by 1 \n\n"
>   ++ " SELECT n, n*n, n*n*n from t_natural where n < 10 order by 1 \n\n"
>   ++ " END;"


> selectMultiResultSet _ = do
>   dbmsname <- inquire InfoDbmsName
>   when (dbmsname == "microsoft sql server") $ do
>     execDrop dropFixtureMultiResultSet3
>     execDrop dropFixtureMultiResultSet2
>     execDrop dropFixtureMultiResultSet1
>     execDDL_ makeFixtureMultiResultSet1
>     execDDL_ makeFixtureMultiResultSet2
>     execDDL_ makeFixtureMultiResultSet3
>     withPreparedStatement (prepareCommand (sql "{call takusenTestProc}")) $ \pstmt -> do
>     withBoundStatement pstmt [] $ \bstmt -> do
>       result1 <- doQuery bstmt iterRS1 []
>       assertEqual "selectMultiResultSet: RS1" [1,4,9,16,25,36,49,64,81] result1
>       result2 <- doQuery (NextResultSet pstmt) iterRS2 []
>       let expect = [(1,1,1),(2,4,8),(3,9,27),(4,16,64),(5,25,125),(6,36,216)
>             ,(7,49,343),(8,64,512),(9,81,729)]
>       assertEqual "selectMultiResultSet: RS2" expect result2
>       return ()
>     execDDL_ dropFixtureMultiResultSet3
>     execDDL_ dropFixtureMultiResultSet2
>     execDDL_ dropFixtureMultiResultSet1
>   where
>     iterRS1 :: (Monad m) => Int -> IterAct m [Int]
>     iterRS1 i acc = result (acc ++ [i])
>     iterRS2 :: (Monad m) => Int -> Int -> Int -> IterAct m [(Int, Int, Int)]
>     iterRS2 i i2 i3 acc = result (acc ++ [(i, i2, i3)])


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

> bindOutput _ = do
>   dbmsname <- liftM (map toLower) (inquire InfoDbmsName)
>   case dbmsname of
>     "microsoft sql server" -> k makeFixtureBindOutputSqlServer
>     "oracle" -> k makeFixtureBindOutputOracle
>     _ -> return ()
>   where
>   k makeFixture = do
>     execDrop dropFixtureBindOutput
>     execDDL_ makeFixture
>     let qry = sqlbind "{call takusenTestProc(?,?)}" [bindP (Out (1234::Int)), bindP (Out (Just "message"))]
>     (x, s) <- doQuery qry iter undefined
>     execDrop dropFixtureBindOutput
>     assertEqual "bindOutput: int " 2468 x
>     assertEqual "bindOutput: string " "output message xxx" s
>   iter :: (Monad m) => Int -> String -> IterAct m (Int, String)
>   iter i s _ = return (Left (i, s))


> testList :: DBLiteralValue a => [a -> DBM mark Session ()]
> testList =
>   selectNoRows :
>   selectTerminatesEarly :
>   selectFloatsAndInts :
>   selectNullString :
>   selectEmptyString :
>   selectUnhandledNull :
>   selectCursor :
>   selectExhaustCursor :
>   selectBindString :
>   selectBindInt :
>   selectBindIntDoubleString :
>   selectBindDate :
>   selectBindBoundaryDates :
>   selectRebindStmt :
>   boundStmtDML :
>   boundStmtDML2 :
>   polymorphicFetchTest :
>   polymorphicFetchTestNull :
>   exceptionRollback :
>   iterateeMatchesResultSet :
>   selectMultiResultSet :
>   bindOutput :
>   []
