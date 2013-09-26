
|
Module      :  Database.PostgreSQL.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable



Can we support database functions that return/create multiple result-sets?
Oracle, MS Sql Server, and Sybase have them,
and we can simulate them in Postgres with the code below.

CREATE TRUSTED PROCEDURAL LANGUAGE 'plpgsql' HANDLER plpgsql_call_handler;

CREATE OR REPLACE VIEW t_whole as
select 0 as n
union select 1
union select 2
union select 3
union select 4
union select 5
union select 6
union select 7
union select 8
union select 9
;

CREATE OR REPLACE VIEW t_natural as
select n from
( select t1.n + 10 * t10.n + 100 * t100.n as n
  from t_whole t1, t_whole t10, t_whole t100
) t
where n > 0
order by 1
;

DROP FUNCTION takusenTestFunc() ;

CREATE OR REPLACE FUNCTION takusenTestFunc() RETURNS SETOF refcursor AS \$\$
DECLARE refc1 refcursor; refc2 refcursor;
BEGIN
    OPEN refc1 FOR SELECT n*n from t_natural where n < 10 order by 1;
    RETURN NEXT refc1;
    OPEN refc2 FOR SELECT n, n*n, n*n*n from t_natural where n < 10 order by 1;
    RETURN NEXT refc2;
END;\$\$ LANGUAGE plpgsql;


-- this select returns two values (rows), both strings (well, refcursors),
-- which are the cursor names.
select * from myfunc();

fetch all from "<unnamed portal 1>";
fetch all from "<unnamed portal 2>";
commit;


Another example:

CREATE OR REPLACE FUNCTION takusenTestFunc(lim int4) RETURNS refcursor AS \$\$
DECLARE
    refc refcursor;
BEGIN
    OPEN refc FOR SELECT n, takusenTestFunc2(n) from t_natural where n < lim order by n;
    RETURN refc;
END;
\$\$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION takusenTestFunc2(lim int4) RETURNS refcursor AS \$\$
DECLARE
    refc refcursor;
BEGIN
    OPEN refc FOR SELECT n from t_natural where n < lim order by n;
    RETURN refc;
END;
\$\$ LANGUAGE plpgsql;

SELECT n, takusenTestFunc(n) from t_natural where n < 10 order by n;


> {-# LANGUAGE OverlappingInstances #-}

> module Database.PostgreSQL.Test.Enumerator (runTest) where

> import qualified Database.PostgreSQL.Test.PGFunctions as Low
> import Database.PostgreSQL.Enumerator
> import Database.Test.Performance as Perf
> import Database.Test.Enumerator
> import Control.Monad (when)
> import Test.MiniUnit
> import Data.Int
> import Data.List
> import Data.Word
> import System.Time


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   putStrLn "PostgreSQL tests"
>   let (user:pswd:dbname:_) = args
>   Low.runTest user
>   flip catchDB basicDBExceptionReporter $ do
>     (r, conn1) <- withContinuedSession (connect [CAuser user]) (testBody runPerf)
>     withSession conn1 testPartTwo

> testBody runPerf = do
>   runFixture PGSqlFunctions
>   when (runPerf == Perf.RunTests) runPerformanceTests

> testPartTwo = do
>   makeFixture execDrop execDDL_
>   destroyFixture execDDL_

> runPerformanceTests = do
>   makeFixture execDrop execDDL_
>   beginTransaction RepeatableRead
>   runTestTT "PostgreSQL performance tests" (map (flip catchDB reportRethrow)
>     [ timedSelect (prefetch 1000 sqlRows2Power20 []) 40 (2^20)
>     , timedSelect (prefetch 1 sqlRows2Power17 []) 40 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power17 []) 5 (2^17)
>     , timedCursor (prefetch 1 sqlRows2Power17 []) 40 (2^17)
>     , timedCursor (prefetch 1000 sqlRows2Power17 []) 5 (2^17)
>     ]
>     )
>   commit
>   destroyFixture execDDL_


> runFixture :: PGSqlFunctions -> DBM mark Session ()
> runFixture fns = do
>   makeFixture execDrop execDDL_
>   execDDL_ makeFixtureNestedMultiResultSet1
>   execDDL_ makeFixtureNestedMultiResultSet2
>   execDDL_ makeFixtureNestedMultiResultSet3
>   execDDL_ makeFixtureNestedMultiResultSet4
>   execDDL_ makeFixtureMultiResultSet1
>   runTestTT "Postgres tests" (map (runOneTest fns) testList)
>   execDDL_ dropFixtureMultiResultSet1
>   execDDL_ dropFixtureNestedMultiResultSet4
>   execDDL_ dropFixtureNestedMultiResultSet3
>   execDDL_ dropFixtureNestedMultiResultSet2
>   execDDL_ dropFixtureNestedMultiResultSet1
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

Note that these two tests use the same prepared statement name.
This tests that the statement is properly deallocated, through use of
withPreparedStatement.

> selectBindString _ = actionBindString
>   (prepareQuery "1" (sql sqlBindString) [bindType "", bindType ""])
>   [bindP "a2", bindP "b1"]

> selectBindInt _ = actionBindInt
>   (prepareQuery "1" (sql sqlBindInt) (map bindType expectBindInt))
>   [bindP (1::Int), bindP (2::Int)]


> selectBindIntDoubleString _ = actionBindIntDoubleString
>   (prefetch 0 sqlBindIntDoubleString [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int), bindP (4.4::Double), bindP "row 2"])

> selectBindDate _ = actionBindDate
>   (prefetch 1 sqlBindDate (map bindP expectBindDate))

> selectBindBool _ = actionBindBool
>   (sqlbind sqlBindBool [bindP True, bindP False])

> str2Word8 :: String -> [Word8]
> str2Word8 s = map (fromIntegral . fromEnum) s
> word8ToStr :: [Word8] -> String
> word8ToStr s = map (toEnum . fromIntegral) s

> selectBindBytea _ = do
>   let input = str2Word8 "\\ \0 ' \255"
>   let expect = input
>   let iterBindBytea :: Monad m => [Word8] -> IterAct m [Word8]
>       iterBindBytea w8 acc = result w8
>   withTransaction Serialisable $ do
>     actual <- doQuery (sqlbind sqlSingleValue [bindP input]) iterBindBytea []
>     assertEqual "selectBindBytea" expect actual

> selectBindUUID _ = do
>   let input = string2uuid "11112222-3333-4444-5555-666677778888"
>   let expect = input
>   let iterBindUUID :: Monad m => UUID -> IterAct m UUID
>       iterBindUUID uuid acc = result uuid
>   withTransaction Serialisable $ do
>     actual <- doQuery (sqlbind sqlSingleValue [bindP input]) iterBindUUID input
>     assertEqual "selectBindUUID" expect actual

> selectBindBoundaryDates _ = actionBindBoundaryDates
>   (prefetch 1 sqlBindBoundaryDates (map bindP expectBoundaryDates))

> selectRebindStmt _ = actionRebind
>   (prepareQuery "1" (sql sqlRebind) [bindType (0::Int)])
>   [bindP (1::Int)] [bindP (2::Int)]

> boundStmtDML _ = actionBoundStmtDML
>   (prepareCommand "boundStmtDML" (sql sqlBoundStmtDML) [bindType (0::Int), bindType ""])
> boundStmtDML2 _ = do
>   beginTransaction RepeatableRead
>   count <- execDML (cmdbind sqlBoundStmtDML [bindP (100::Int), bindP "100"])
>   rollback
>   assertEqual sqlBoundStmtDML 1 count


> polymorphicFetchTest _ = actionPolymorphicFetch
>   (prefetch 0 sqlPolymorphicFetch [bindP expectPolymorphicFetch])

> polymorphicFetchTestNull _ = withTransaction RepeatableRead $ 
>   actionPolymorphicFetchNull (prefetch 1 sqlPolymorphicFetchNull [])

For the exceptionRollback test we have to specify the count result is int4;
if we don't specify the type then it defaults to a Postgres numeric,
which we can't yet marshal.

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4
>   ("select count(*)::int4 from " ++ testTable)


Ensure we get an exception if the result-set columns
don't match the output buffers (at present we only
check that there aren't too many buffers
i.e. not enough columns).

> iterateeMatchesResultSet _ = actionIterateeMatchesResultSet
>   (prefetch 0 sqlIterateeMatchesResultSet [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int)])

> selectUTF8Text _ = do
>   let iter (s::String) (_::String) = result s
>   -- GREEK SMALL LETTER PHI : CF86 UTF8, 03C6 UTF16, 966 decimal
>   let expect = ['\966']
>   result <- doQuery (sql ("select '" ++ expect ++ "'")) iter ""
>   assertEqual "selectUTF8Text" expect result


> dropFixtureMultiResultSet1 = "DROP FUNCTION takusenTestFunc()"
> makeFixtureMultiResultSet1 =
>   "CREATE OR REPLACE FUNCTION takusenTestFunc() RETURNS SETOF refcursor AS $$"
>   ++ "DECLARE refc1 refcursor; refc2 refcursor; BEGIN"
>   ++ "    OPEN refc1 FOR SELECT n*n from t_natural where n < 10 order by 1;"
>   ++ "    RETURN NEXT refc1;"
>   ++ "    OPEN refc2 FOR SELECT n, n*n, n*n*n from t_natural where n < 10 order by 1;"
>   ++ "    RETURN NEXT refc2;"
>   ++ "END;$$ LANGUAGE plpgsql;\n"

> selectMultiResultSet _ = do
>   withTransaction RepeatableRead $ do
>   withPreparedStatement (prepareLargeQuery 2 "stmt1" (sql "select * from takusenTestFunc()") []) $ \pstmt -> do
>   withBoundStatement pstmt [] $ \bstmt -> do
>     dummy <- doQuery bstmt iterMain []
>     result1 <- doQuery (NextResultSet pstmt) iterRS1 []
>     assertEqual "selectMultiResultSet: RS1" [1,4,9,16,25,36,49,64,81] result1
>     result2 <- doQuery (NextResultSet pstmt) iterRS2 []
>     let expect = [(1,1,1),(2,4,8),(3,9,27),(4,16,64),(5,25,125),(6,36,216)
>           ,(7,49,343),(8,64,512),(9,81,729)]
>     assertEqual "selectMultiResultSet: RS2" expect result2
>     return ()
>   where
>     iterMain :: (Monad m) => (RefCursor String) -> IterAct m [RefCursor String]
>     iterMain c acc = result (acc ++ [c])
>     iterRS1 :: (Monad m) => Int -> IterAct m [Int]
>     iterRS1 i acc = result (acc ++ [i])
>     iterRS2 :: (Monad m) => Int -> Int -> Int -> IterAct m [(Int, Int, Int)]
>     iterRS2 i i2 i3 acc = result (acc ++ [(i, i2, i3)])



> dropFixtureNestedMultiResultSet1 = "DROP VIEW t_whole"
> makeFixtureNestedMultiResultSet1 = "CREATE OR REPLACE VIEW t_whole as"
>   ++ "  select 0 as n union select 1 union select 2 union select 3 union select 4"
>   ++ " union select 5 union select 6 union select 7 union select 8 union select 9"

> dropFixtureNestedMultiResultSet2 = "DROP VIEW t_natural"
> makeFixtureNestedMultiResultSet2 = "CREATE OR REPLACE VIEW t_natural as"
>   ++ " select n from"
>   ++ " ( select t1.n + 10 * t10.n + 100 * t100.n as n"
>   ++ "   from t_whole t1, t_whole t10, t_whole t100"
>   ++ " ) t where n > 0 order by 1"

> dropFixtureNestedMultiResultSet3 = "DROP FUNCTION takusenTestFunc(int4)"
> makeFixtureNestedMultiResultSet3 =
>      "CREATE OR REPLACE FUNCTION takusenTestFunc(lim int4) RETURNS refcursor AS $$"
>   ++ " DECLARE refc refcursor; BEGIN"
>   ++ "     OPEN refc FOR SELECT n, takusenTestFunc2(n) from t_natural where n < lim order by n;"
>   ++ "     RETURN refc; END; $$ LANGUAGE plpgsql;"

> dropFixtureNestedMultiResultSet4 = "DROP FUNCTION takusenTestFunc2(int4)"
> makeFixtureNestedMultiResultSet4 =
>      "CREATE OR REPLACE FUNCTION takusenTestFunc2(lim int4) RETURNS refcursor AS $$"
>   ++ " DECLARE refc refcursor; BEGIN"
>   ++ "     OPEN refc FOR SELECT n from t_natural where n < lim order by n;"
>   ++ "     RETURN refc; END; $$ LANGUAGE plpgsql;"

> selectNestedMultiResultSet _ = do
>   let
>     q = "SELECT n, takusenTestFunc(n) from t_natural where n < 10 order by n"
>     iterMain   (i::Int) (c::RefCursor String) acc = result' ((i,c):acc)
>     iterInner  (i::Int) (c::RefCursor String) acc = result' ((i,c):acc)
>     iterInner2 (i::Int) acc = result' (i:acc)
>   withTransaction RepeatableRead $ do
>     rs <- doQuery (sql q) iterMain []
>     assertEqual "selectNestedMultiResultSet" [9,8,7,6,5,4,3,2,1] (map fst rs)
>     --print_ ""
>     flip mapM_ rs $ \(outer, c) -> do
>       rs <- doQuery c iterInner []
>       let expect = drop (9-outer) [8,7,6,5,4,3,2,1]
>       assertEqual "processOuter" expect (map fst rs)
>       flip mapM_ rs $ \(inner, c) -> do
>         rs <- doQuery c iterInner2 []
>         let expect = drop (9-inner) [8,7,6,5,4,3,2,1]
>         assertEqual "processInner" expect rs
>         flip mapM_ rs $ \i -> do
>           --print_ (show outer ++ " " ++ show inner ++ " " ++ show i)
>           assertBool "processInner2" (i < inner)



> generateErrorMessageTest _ = do
>   catchDB ( do
>       doQuery (sql "select * from nonExistantObject") iterNoRows []
>       return ()
>     ) (\e -> do
>       let msg = formatDBException e
>       -- uncomment this to view error message
>       --liftIO (putStrLn msg)
>       let expect = "42P01 7: ERROR:  42P01: relation"
>       assertEqual "generateErrorMessageTest" expect (take (length expect) msg)
>     )


> testList :: [PGSqlFunctions -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull
>   , selectNullDate, selectDate, selectCalDate, selectBoundaryDates
>   , selectCursor, selectExhaustCursor, selectBindBytea, selectBindUUID
>   , selectBindString, selectBindInt, selectBindIntDoubleString
>   , selectBindDate, selectBindBool, selectBindBoundaryDates
>   , selectRebindStmt, boundStmtDML, boundStmtDML2
>   , polymorphicFetchTest, polymorphicFetchTestNull, exceptionRollback
>   , selectMultiResultSet, selectNestedMultiResultSet
>   , generateErrorMessageTest, selectUTF8Text, iterateeMatchesResultSet
>   ]
