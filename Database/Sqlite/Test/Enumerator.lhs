
|
Module      :  Database.Sqlite.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


> {-# LANGUAGE OverlappingInstances #-}

> module Database.Sqlite.Test.Enumerator (runTest) where

> import qualified Database.Sqlite.Test.SqliteFunctions as Low
> import Database.Sqlite.Enumerator
> import Database.Test.Performance as Perf
> import Database.Test.Enumerator
> import Control.Monad (when)
> import Control.Monad.Trans (liftIO)
> import Test.MiniUnit
> import Data.Int
> import System.Time


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   let (user:pswd:dbname:_) = args
>   Low.runTest dbname
>   flip catchDB basicDBExceptionReporter $ do
>     (r, conn1) <- withContinuedSession (connect dbname) (testBody runPerf)
>     withSession conn1 testPartTwo

> testBody runPerf = do
>   runFixture SqliteFunctions
>   when (runPerf == Perf.RunTests) runPerformanceTests

> testPartTwo :: DBM mark Session ()
> testPartTwo = do
>   makeFixture execDrop execDDL_
>   destroyFixture execDDL_

> runPerformanceTests = do
>   makeFixture execDrop execDDL_
>   beginTransaction RepeatableRead
>   {-# SCC "runPerformanceTests:runTestTT" #-}
>    runTestTT "Sqlite performance tests" (map (flip catchDB reportRethrow)
>     [ timedSelect (prefetch 1000 sqlRows2Power20 []) 35 (2^20)
>     , timedSelect (prefetch 1 sqlRows2Power17 []) 4 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power17 []) 4 (2^17)
>     , timedCursor (prefetch 1 sqlRows2Power17 []) 4 (2^17)
>     , timedCursor (prefetch 1000 sqlRows2Power17 []) 4 (2^17)
>     ]
>     )
>   commit
>   destroyFixture execDDL_


> runFixture :: DBLiteralValue a => a -> DBM mark Session ()
> runFixture fns = do
>   makeFixture execDrop execDDL_
>   runTestTT "Sqlite tests" (map (runOneTest fns) testList)
>   destroyFixture execDDL_

> runOneTest fns t = catchDB (t fns) reportRethrow

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

> selectDate dateFn = selectTest (sqlDate dateFn) iterDate expectDate

> selectCalDate dateFn = selectTest (sqlDate dateFn) iterCalDate expectCalDate

> selectBoundaryDates dateFn = selectTest (sqlBoundaryDates dateFn) iterBoundaryDates expectBoundaryDates

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

> selectBindBoundaryDates _ = actionBindBoundaryDates
>   (prefetch 1 sqlBindBoundaryDates (map bindP expectBoundaryDates))

> selectRebindStmt _ = actionRebind (prepareQuery (sql sqlRebind))
>    [bindP (1::Int)] [bindP (2::Int)]

> boundStmtDML _ = actionBoundStmtDML (prepareCommand (sql sqlBoundStmtDML))
> boundStmtDML2 _ = do
>   -- cannot use withTransaction with rollback/commit;
>   -- if you explicitly end the transaction, then when withTransaction
>   -- attempts to end it (with a commit, in the success case) then we
>   -- get a "logic error".
>   -- This differs from PostgreSQL and Oracle, who don't seem to care if
>   -- you commit or rollback too many times.
>   beginTransaction RepeatableRead
>   count <- execDML (cmdbind sqlBoundStmtDML [bindP (100::Int), bindP "100"])
>   rollback
>   assertEqual sqlBoundStmtDML 1 count

> polymorphicFetchTest _ = actionPolymorphicFetch
>   (prefetch 1 sqlPolymorphicFetch [bindP expectPolymorphicFetch])

> polymorphicFetchTestNull _ = actionPolymorphicFetchNull
>   (prefetch 1 sqlPolymorphicFetchNull [])

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4 sqlExceptionRollback

> insertGetRowId _ =
>   withTransaction RepeatableRead ( do
>     execDML (sql ("insert into " ++ testTable ++ " values (100, '100')"))
>     rowid <- inquire LastInsertRowid
>     assertBool "insertGetRowId" (rowid > 0)
>     --liftIO $ putStrLn ("last insert row id " ++ show rowid)
>   )

> testList :: DBLiteralValue a => [a -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull
>   , selectNullDate, selectDate, selectCalDate, selectBoundaryDates
>   , selectCursor, selectExhaustCursor
>   , selectBindString, selectBindInt, selectBindIntDoubleString
>   , selectBindDate, selectBindBoundaryDates, selectRebindStmt
>   , boundStmtDML, boundStmtDML2
>   , polymorphicFetchTest, polymorphicFetchTestNull, exceptionRollback
>   , insertGetRowId
>   ]
