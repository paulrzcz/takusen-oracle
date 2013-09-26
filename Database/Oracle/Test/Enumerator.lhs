
|
Module      :  Database.Oracle.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


The Oracle multi-result-set tests require these database objects:

CREATE OR REPLACE VIEW t_whole as
select 0 as n  from dual union select 1 from dual
union select 2 from dual union select 3 from dual
union select 4 from dual union select 5 from dual
union select 6 from dual union select 7 from dual
union select 8 from dual union select 9 from dual
;

CREATE OR REPLACE VIEW t_natural as
select n from
( select t1.n + 10 * t10.n + 100 * t100.n as n
from t_whole t1, t_whole t10, t_whole t100
) t where n > 0 order by 1
;

create or replace package Takusen as type RefCursor is ref cursor; end;

CREATE OR REPLACE PROCEDURE takusenTestProc
(refc1 out Takusen.RefCursor, refc2 out Takusen.RefCursor) AS BEGIN
OPEN refc1 FOR SELECT n*n from t_natural where n < 10 order by 1;
OPEN refc2 FOR SELECT n, n*n, n*n*n from t_natural where n < 10 order by 1;
END;

select n, cursor(
  SELECT nat2.n
  , cursor(SELECT nat3.n from t_natural nat3 where nat3.n < nat2.n order by n)
  from t_natural nat2 where nat2.n < nat.n order by n
)
from t_natural nat where n < 10 order by n;


> {-# LANGUAGE OverlappingInstances #-}

> module Database.Oracle.Test.Enumerator (runTest) where

> import qualified Database.Oracle.Test.OCIFunctions as Low
> import Database.Oracle.Enumerator
> import Database.Util (print_)
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
>   Low.runTest args
>   flip catchDB basicDBExceptionReporter $
>     withSession (connect user pswd dbname) (testBody runPerf)

> testBody runPerf = do
>   runFixture OracleFunctions
>   when (runPerf == Perf.RunTests) runPerformanceTests

> runPerformanceTests = do
>   makeFixture execDrop execDDL_
>   beginTransaction RepeatableRead
>   runTestTT "Oracle performance tests" (map (flip catchDB reportRethrow)
>     [ timedSelect (prefetch 1000 sqlRows2Power20 []) 30 (2^20)
>     , timedSelect (prefetch 1 sqlRows2Power17 []) 60 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power17 []) 6 (2^17)
>     , timedCursor (prefetch 1 sqlRows2Power17 []) 60 (2^17)
>     , timedCursor (prefetch 1000 sqlRows2Power17 []) 6 (2^17)
>     ]
>     )
>   commit
>   destroyFixture execDDL_


> runFixture :: OracleFunctions -> DBM mark Session ()
> runFixture fns = do
>   makeFixture execDrop execDDL_
>   execDDL_ makeFixtureMultiResultSet1
>   execDDL_ makeFixtureMultiResultSet2
>   execDDL_ makeFixtureMultiResultSet3
>   execDDL_ makeFixtureMultiResultSet4
>   execDDL_ makeFixtureBindOutput
>   runTestTT "Oracle tests" (map (runOneTest fns) testList)
>   execDDL_ dropFixtureBindOutput
>   execDDL_ dropFixtureMultiResultSet4
>   execDDL_ dropFixtureMultiResultSet3
>   execDDL_ dropFixtureMultiResultSet2
>   execDDL_ dropFixtureMultiResultSet1
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
>   (prefetch 0 sqlBindIntDoubleString [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int), bindP (4.4::Double), bindP "row 2"])

> selectBindDate _ = actionBindDate
>   (prefetch 1 sqlBindDate (map bindP expectBindDate))

> selectBindBoundaryDates _ = actionBindBoundaryDates
>   (prefetch 1 sqlBindBoundaryDates (map bindP expectBoundaryDates))

> selectRebindStmt _ = actionRebind (prepareQuery (sql sqlRebind))
>    [bindP (1::Int)] [bindP (2::Int)]

> boundStmtDML _ = actionBoundStmtDML (prepareCommand (sql sqlBoundStmtDML))
> boundStmtDML2 _ = withTransaction RepeatableRead $ do
>   count <- execDML (cmdbind sqlBoundStmtDML [bindP (100::Int), bindP "100"])
>   assertEqual sqlBoundStmtDML 1 count
>   rollback


> polymorphicFetchTest _ = actionPolymorphicFetch
>   (prefetch 0 sqlPolymorphicFetch [bindP expectPolymorphicFetch])

> polymorphicFetchTestNull _ = actionPolymorphicFetchNull
>   (prefetch 1 sqlPolymorphicFetchNull [])

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4 sqlExceptionRollback


FIXME  check definitions of whole and natural numbers

> dropFixtureMultiResultSet1 = "DROP VIEW t_whole"
> makeFixtureMultiResultSet1 = "CREATE OR REPLACE VIEW t_whole as"
>   ++ " select 0 as n  from dual union select 1 from dual"
>   ++ " union select 2 from dual union select 3 from dual"
>   ++ " union select 4 from dual union select 5 from dual"
>   ++ " union select 6 from dual union select 7 from dual"
>   ++ " union select 8 from dual union select 9 from dual"

> dropFixtureMultiResultSet2 = "DROP VIEW t_natural"
> makeFixtureMultiResultSet2 = "CREATE OR REPLACE VIEW t_natural as"
>   ++ " select n from"
>   ++ " ( select t1.n + 10 * t10.n + 100 * t100.n as n"
>   ++ "   from t_whole t1, t_whole t10, t_whole t100"
>   ++ " ) t where n > 0 order by 1"


> dropFixtureMultiResultSet3 = "DROP package Takusen"
> makeFixtureMultiResultSet3 =
>   "create or replace package Takusen as type RefCursor is ref cursor; end;"

> dropFixtureMultiResultSet4 = "DROP PROCEDURE takusenTestProc"
> makeFixtureMultiResultSet4 = "CREATE OR REPLACE PROCEDURE takusenTestProc"
>   ++ " (refc1 out Takusen.RefCursor, refc2 out Takusen.RefCursor) AS BEGIN"
>   ++ " OPEN refc1 FOR SELECT n*n from t_natural where n < 10 order by 1;"
>   ++ " OPEN refc2 FOR SELECT n, n*n, n*n*n from t_natural where n < 10 order by 1;"
>   ++ " END;"

What would you need to do to make database functions and procedures
callable from Haskell as if they were local functions (IO actions)?

wrapPLSQLFunc funcname parms =
  let
     sqltext = "begin " ++ (head args) ++ " := " ++ funcname ++ "(" ++ placeholders ++ "); end;"
     placeholders = concat (intersperse "," (tail args))
     args = take (length parms) (map (\n -> ":x" ++ show n) [1..])
  in cmdbind sqltext parms

wrapPLSQLProc procname parms =
  let
     sqltext = "begin " ++ procname ++ "(" ++ placeholders ++ "); end;"
     placeholders = concat (intersperse "," args)
     args = take (length parms) (map (\n -> ":x" ++ show n) [1..])
  in cmdbind sqltext parms

convertCcy :: String -> Double -> String -> UTCTime -> DBM mark Session (String, Double)
convertCcy ccyFrom valFrom ccyTo onDate = do
  sqlcmd = wrapPLSQLFunc "pk_fx.convert_ccy"
    [ bindP (Out (0 :: Double))
    , bindP ccyFrom
    , bindP valFrom
    , bindP ccyTo
    , bindP onDate
    ]
  let
    iter :: Monad m => Double -> IterAct m Double
    iter val seed = return (Left val)
  result = doQuery sqlcmd iter undefined
  return result

> selectMultiResultSet _ = do
>   let refcursor :: Maybe StmtHandle; refcursor = Just undefined
>   withTransaction RepeatableRead $ do
>   withPreparedStatement (prepareCommand (sql "begin takusenTestProc(:1,:2); end;")) $ \pstmt -> do
>   withBoundStatement pstmt [bindP (Out refcursor), bindP (Out refcursor)] $ \bstmt -> do
>     dummy <- doQuery bstmt iterMain ()
>     result1 <- doQuery (NextResultSet pstmt) iterRS1 []
>     assertEqual "selectMultiResultSet: RS1" [1,4,9,16,25,36,49,64,81] result1
>     result2 <- doQuery (NextResultSet pstmt) iterRS2 []
>     let expect = [(1,1,1),(2,4,8),(3,9,27),(4,16,64),(5,25,125),(6,36,216)
>           ,(7,49,343),(8,64,512),(9,81,729)]
>     assertEqual "selectMultiResultSet: RS2" expect result2
>     return ()
>   where
>     iterMain :: (Monad m) => RefCursor StmtHandle -> RefCursor StmtHandle -> IterAct m ()
>     iterMain c1 c2 acc = return (Left acc)
>     iterRS1 :: (Monad m) => Int -> IterAct m [Int]
>     iterRS1 i acc = result (acc ++ [i])
>     iterRS2 :: (Monad m) => Int -> Int -> Int -> IterAct m [(Int, Int, Int)]
>     iterRS2 i i2 i3 acc = result (acc ++ [(i, i2, i3)])


> selectNestedMultiResultSet :: OracleFunctions -> DBM mark Session ()
> selectNestedMultiResultSet _ = do
>   let
>     -- This returns two rows, each of which contains one cursor.
>     -- The first cursor returns 101, the second 102.
>     q = "select cursor(select n from dual) from"
>       ++ " (select 101 as n from dual union select 102 from dual)"
>     iterMain (c::RefCursor StmtHandle) acc = do
>       rs <- doQuery c iterInner []
>       result' (c:acc)
>     iterInner (i::Int) acc = do
>       if (i /= 101 && i /= 102)
>         then assertFailure "selectNestedMultiResultSet: inner value not 101 or 102"
>         else return ()
>       result' (i:acc)
>   withTransaction RepeatableRead $ do
>   withPreparedStatement (prepareQuery (sql q)) $ \pstmt -> do
>   withBoundStatement pstmt [] $ \bstmt -> do
>       rs <- doQuery bstmt iterMain []
>       return ()


> selectNestedMultiResultSet2 :: OracleFunctions -> DBM mark Session ()
> selectNestedMultiResultSet2 _ = do
>   let
>     q = "select n, cursor(SELECT nat2.n, cursor"
>         ++ "     (SELECT nat3.n from t_natural nat3 where nat3.n < nat2.n order by n)"
>         ++ "   from t_natural nat2 where nat2.n < nat.n order by n)"
>         ++ " from t_natural nat where n < 10 order by n"
>     iterMain   (outer::Int) (c::RefCursor StmtHandle) acc = do
>       rs <- doQuery c (iterInner outer) []
>       let expect = drop (9-outer) [8,7,6,5,4,3,2,1]
>       assertEqual "processOuter" expect (map fst rs)
>       result' ((outer,c):acc)
>     iterInner outer (inner::Int) (c::RefCursor StmtHandle) acc = do
>       rs <- doQuery c (iterInner2 outer inner) []
>       let expect = drop (9-inner) [8,7,6,5,4,3,2,1]
>       assertEqual "processInner" expect rs
>       result' ((inner,c):acc)
>     iterInner2 outer inner (i::Int) acc = do
>       --print_ (show outer ++ " " ++ show inner ++ " " ++ show i)
>       assertBool "processInner2" (i < inner)
>       result' (i:acc)
>   withTransaction RepeatableRead $ do
>   withPreparedStatement (prepareQuery (sql q)) $ \pstmt -> do
>   withBoundStatement pstmt [] $ \bstmt -> do
>       rs <- doQuery bstmt iterMain []
>       assertEqual "selectNestedMultiResultSet" [9,8,7,6,5,4,3,2,1] (map fst rs)
>       --print_ ""

> selectNestedMultiResultSet3 :: OracleFunctions -> DBM mark Session ()
> selectNestedMultiResultSet3 _ = do
>   let
>     q = "select n, cursor(SELECT nat2.n, cursor"
>         ++ "     (SELECT nat3.n from t_natural nat3 where nat3.n < nat2.n order by n)"
>         ++ "   from t_natural nat2 where nat2.n < nat.n order by n)"
>         ++ " from t_natural nat where n < 10 order by n"
>     iterMain   (outer::Int) (c::RefCursor StmtHandle) acc = do
>       rs <- doQuery c (iterInner outer) []
>       let expect = drop (9-outer) [8,7,6,5,4,3,2,1]
>       assertEqual "processOuter" expect (map fst rs)
>       result' ((outer,c):acc)
>     iterInner outer (inner::Int) (c::RefCursor StmtHandle) acc = do
>       rs <- doQuery c (iterInner2 outer inner) []
>       let expect = drop (9-inner) [8,7,6,5,4,3,2,1]
>       assertEqual "processInner" expect rs
>       result' ((inner,c):acc)
>     iterInner2 outer inner (i::Int) acc = do
>       --print_ (show outer ++ " " ++ show inner ++ " " ++ show i)
>       assertBool "processInner2" (i < inner)
>       result' (i:acc)
>   withTransaction RepeatableRead $ do
>       rs <- doQuery (sql q) iterMain []
>       assertEqual "selectNestedMultiResultSet" [9,8,7,6,5,4,3,2,1] (map fst rs)
>       --print_ ""

> dropFixtureBindOutput = "DROP PROCEDURE takusenTestBindProc"

> makeFixtureBindOutput = "CREATE or replace PROCEDURE takusenTestBindProc(x in out number, y in out varchar2)"
>   ++ " AS BEGIN\n"
>   ++ " y := 'output ' || y;\n"
>   ++ " x := x * 2;\n"
>   ++ " END;"

> bindOutputString _ = do
>   let sqltext = "begin takusenTestBindProc(:1,:2); end;"
>   let qry = cmdbind sqltext [bindP (Out (1234::Int)), bindP (Out (Just "message"))]
>   (x, s) <- doQuery qry iter undefined
>   assertEqual "bindOutputString: int " 2468 x
>   assertEqual "bindOutputString: string " "output message" s
>   where
>     iter :: (Monad m) => Int -> String -> IterAct m (Int, String)
>     iter i s _ = return (Left (i, s))


> bindOutput _ = do
>   let sqltext =
>         "begin :1 := :1 + 1;\n "
>         ++ ":2 := :2 * 2;\n"
>         ++ "end;"
>   let qry = cmdbind sqltext [bindP (Out (44.4 :: Double)), bindP (Out (1234::Int))]
>   (d, i) <- doQuery qry iter undefined
>   assertEqual "bindOutput: double " 45.4 d
>   assertEqual "bindOutput: int " 2468 i
>   where
>     iter :: (Monad m) => Double -> Int -> IterAct m (Double, Int)
>     iter d i _ = return (Left (d, i))


> testList :: [OracleFunctions -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull
>   , selectNullDate, selectDate, selectCalDate, selectBoundaryDates
>   , selectCursor, selectExhaustCursor
>   , selectBindString, selectBindInt, selectBindIntDoubleString
>   , selectBindDate, selectBindBoundaryDates, selectRebindStmt
>   , boundStmtDML, boundStmtDML2
>   , polymorphicFetchTest, polymorphicFetchTestNull, exceptionRollback
>   , selectMultiResultSet, selectNestedMultiResultSet
>   , selectNestedMultiResultSet2, selectNestedMultiResultSet3
>   , bindOutput, bindOutputString
>   ]
