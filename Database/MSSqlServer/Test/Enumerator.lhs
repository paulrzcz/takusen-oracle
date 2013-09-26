
|
Module      :  Database.MSSqlServer.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.MSSqlServer.Test.Enumerator (runTest) where

> --import qualified Database.MSSqlServer.Enumerator as MSSql (connect, disconnect)
> import qualified Database.MSSqlServer.Test.MSSqlFunctions as Low
> import Database.Test.Enumerator as Enum
> import Database.Test.Performance as Perf
> import Database.Enumerator
> import Control.Monad (when)


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = catchDB ( do
>     let [user, pswd, dbname ] = args
>     Low.runTest args
>     {-
>     sess <- MSSql.connect user pswd dbname
>     Enum.runTests dateMSSql sess
>     when (runPerf == Perf.RunTests) (Perf.runTests sess)
>     MSSql.disconnect sess
>     -}
>   ) basicDBExceptionReporter
