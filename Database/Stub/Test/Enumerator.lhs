
|
Module      :  Database.Stub.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Simple test harness for Stub.
Stub can't share the tests for \"real\" backends because it
returns a somewhat contrived result set.


> {-# LANGUAGE OverlappingInstances #-}

> module Database.Stub.Test.Enumerator (runTest) where

> import Database.Enumerator
> import Database.Stub.Enumerator
> -- import Database.Test.Performance as Perf
> import System.Time  -- CalendarTime
> import Test.MiniUnit
> import Data.Int
> import Control.Monad.Trans (liftIO)




> runTest :: a -> [String] -> IO ()
> runTest _ _ =
>   withSession (connect (ConnParm "" "" ""))
>     (runTestTT "Stub tests" (map runOneTest testList) >> return ())

> runOneTest t = catchDB (t ()) reportRethrow

> testList :: [() -> DBM mark Session ()]
> testList =
>   [ selectString, selectIterIO, selectFloatInt
>   , selectStringNullInt, selectDatetime
>   , selectCursor, selectExhaustCursor
>   ]

> selectTest iter expect = catchDB ( do
>     actual <- doQuery (sql "") iter []
>     liftIO $ assertEqual "" expect actual
>   ) (\e -> return () )
> selectTest' iter expect = catchDB ( do
>     actual <- doQuery (prefetch 10 "") iter []
>     liftIO $ assertEqual "" expect actual
>   ) (\e -> return () )

> selectString () = selectTest iter expect
>   where
>     -- To illustrate that the full signature is not necessary as long
>     -- as some type information (e.g., (c1::String)) is provided --
>     -- or enough context for the compiler to figure that out.
>     -- iter :: (Monad m) => String -> IterAct m [String]
>     -- iter :: String -> IterAct (DBM mark Session) [String]
>     iter (c1::String) acc = result $ c1:acc
>     expect = [ "boo", "boo", "boo" ]


The following test illustrates doing IO in the iteratee itself.

> -- monomorphism restriction strikes...
> selectIterIO () = selectTest iter ([]::[String])
>   where
>     -- Here, it's better to avoid the signature and specify
>     -- types of the arguments specifically so we do not have to
>     -- figure out which exactly Monad we're operating in.
>     -- We let the compiler figure out the right monad from
>     -- the context (that is, from the Session)
>     --iter :: String -> IterAct  (ReaderT Query SessionQuery) ()
>     iter (c1::String) seed = do
>       ct <- liftIO $ getClockTime
>       liftIO $ putStr "<look: IO>"
>       result seed


> selectFloatInt () = selectTest' iter expect
>   where
>     iter :: (Monad m) => Double -> Int -> IterAct m [(Double, Int)]
>     iter c1 c2 acc = result $ (c1, c2):acc
>     expect = [ (1.1, 1), (1.1, 1), (1.1, 1) ]



> selectStringNullInt () = selectTest' iter expect
>   where
>     iter :: (Monad m) =>
>       String -> Maybe Int -> IterAct m [(String, Int)]
>     iter c1 c2 acc = result $ (c1, ifNull c2 (-(1))):acc
>     expect = [ ("boo", 1), ("boo", -1), ("boo", 1) ]


> defDate = makeCalTime 19710701120101

> selectDatetime () = selectTest iter expect
>   where
>     iter :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
>     iter c1 acc = result $ c1:acc
>     expect = [ defDate, defDate, defDate ]



> selectCursor () = do
>   let
>     iter :: (Monad m) => Maybe Int -> IterAct m [Int]
>     iter i acc = result $ (ifNull i 2):acc
>   withCursor (sql "") iter [] $ \c -> do
>     r <- cursorCurrent c
>     liftIO $ assertEqual "selectCursor" [1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual "selectCursor" False doneBool
>     --
>     cursorNext c
>     r <- cursorCurrent c
>     liftIO $ assertEqual "selectCursor" [2, 1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual "selectCursor" False doneBool
>     --
>     cursorNext c
>     r <- cursorCurrent c
>     liftIO $ assertEqual "selectCursor" [1, 2, 1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual "selectCursor" False doneBool
>     --
>     cursorNext c
>     r <- cursorCurrent c
>     liftIO $ assertEqual "selectCursor" [1, 2, 1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual "selectCursor" True doneBool
>     --
>     return ()



> selectExhaustCursor () = catchDB (do
>     let
>     -- Again, here we demonstrate the use of a local argument
>     -- type annotation rather than the complete signature.
>     -- Let the compiler figure out the monad and the seed type.
>     --iter :: (Maybe m) => Maybe Int -> IterAct m [Int]
>       iter (i::Maybe Int) acc = result $ (ifNull i (-(1))):acc
>     withCursor (sql "") iter [] $ \c -> do
>         cursorNext c
>         cursorNext c
>         cursorNext c
>         cursorNext c
>         liftIO $ assertFailure "selectExhaustCursor"
>         return ()
>     ) (\e -> return () )


> makeCalTime :: Int64 -> CalendarTime
> makeCalTime i =
>   let
>     year = (i `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>   in CalendarTime
>     { ctYear = fromIntegral year
>     , ctMonth = toEnum (fromIntegral month - 1)
>     , ctDay = fromIntegral day
>     , ctHour = fromIntegral hour
>     , ctMin = fromIntegral minute
>     , ctSec = fromIntegral second
>     , ctPicosec = 0
>     , ctWDay = Sunday
>     , ctYDay = -1
>     , ctTZName = "UTC"
>     , ctTZ = 0
>     , ctIsDST = False
>     }

