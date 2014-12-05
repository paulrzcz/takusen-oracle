
|
Module      :  Database.Test.Performance
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Performance tests. Currently just tests large result sets.


> {-# LANGUAGE OverlappingInstances #-}

> module Database.Test.Performance where

> import Database.Enumerator
> import Database.Util
> import System.Environment (getArgs)
> import Control.Monad
> import System.Time
> import Test.MiniUnit
> import Control.Monad.Reader

> data ShouldRunTests = RunTests | Don'tRunTests deriving (Show, Eq)

> timedCursor stmt limit rows = do
>   withCursor stmt (rowCounter rows) 0 $ \c -> do
>     ct1 <- liftIO getClockTime
>     replicateM_ rows (cursorNext c)
>     ct2 <- liftIO getClockTime
>     let diffCt = diffClockTimes ct2 ct1
>     let limitDiff = TimeDiff 0 0 0 0 0 limit 0
>     print_ $ "timedCursor: " ++ timeDiffToString diffCt
>     assertBool ("timedCursor: time " ++ (timeDiffToString diffCt) ++ " (limit " ++ (timeDiffToString limitDiff) ++ ")")
>       (diffCt < limitDiff)


> timedSelect stmt limit rows = do
>   ct1 <- liftIO getClockTime
>   r <- doQuery stmt (rowCounter rows) 0
>   ct2 <- liftIO getClockTime
>   let diffCt = diffClockTimes ct2 ct1
>   -- assume it must complete within limit seconds
>   let limitDiff = TimeDiff 0 0 0 0 0 limit 0
>   print_ $ "timedSelect: " ++ timeDiffToString diffCt
>   assertBool ("timedSelect: time " ++ (timeDiffToString diffCt) ++ " (limit " ++ (timeDiffToString limitDiff) ++ ")")
>     (diffCt < limitDiff)
>   assertEqual ("timedSelect: rows " ++ (show r)) rows r


|This counter takes the maximum number of rows to fetch as its first argument,
so don't forget to curry it when using it as an iteratee function.
We also try to ensure that it is strict in the counter;
we don't want thousands or millions of unevaluated '+' thunks sitting
on the stack.

> rowCounter :: (Monad m) => Int -> Int -> IterAct m Int
> rowCounter n _ i = result' (i+1)
>   --if i >= n then return (Left $! i) else return (Right $! (1 + i))
>   --if i >= n then return (Left i) else return (Right (1 + i))


2 ^ 16 = 65536
2 ^ 20 = 1048576

Cartesian product. Each instance of the subquery multiplies
the total number of rows by 2, so the number of rows
is 2 ^ (count of subquery instances).
You start to notice the pause around 2^13,
and 2^16 blows out the standard 1M stack
if you use the lazy version of result. Bummer.

> sqlRows2Power17 :: String
> sqlRows2Power17 = 
>   "select 1 from"
>   ++ "  ( select 1 x from tdual union select 0 from tdual) t1"
>   ++ ", ( select 2 x from tdual union select 0 from tdual) t2"
>   ++ ", ( select 3 x from tdual union select 0 from tdual) t3"
>   ++ ", ( select 4 x from tdual union select 0 from tdual) t4"
>   ++ ", ( select 5 x from tdual union select 0 from tdual) t5"
>   ++ ", ( select 6 x from tdual union select 0 from tdual) t6"
>   ++ ", ( select 7 x from tdual union select 0 from tdual) t7"
>   ++ ", ( select 8 x from tdual union select 0 from tdual) t8"
>   ++ ", ( select 9 x from tdual union select 0 from tdual) t9"
>   ++ ", ( select 10 x from tdual union select 0 from tdual) t10"
>   ++ ", ( select 11 x from tdual union select 0 from tdual) t11"
>   ++ ", ( select 12 x from tdual union select 0 from tdual) t12"
>   ++ ", ( select 13 x from tdual union select 0 from tdual) t13"
>   ++ ", ( select 14 x from tdual union select 0 from tdual) t14"
>   ++ ", ( select 15 x from tdual union select 0 from tdual) t15"
>   ++ ", ( select 16 x from tdual union select 0 from tdual) t16"
>   ++ ", ( select 17 x from tdual union select 0 from tdual) t17"

> sqlRows2Power20 :: String
> sqlRows2Power20 = sqlRows2Power17
>   ++ ", ( select 18 x from tdual union select 0 from tdual) t18"
>   ++ ", ( select 19 x from tdual union select 0 from tdual) t19"
>   ++ ", ( select 20 x from tdual union select 0 from tdual) t20"
